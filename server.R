# shiny server
shinyServer(function(input, output, session) {
  read_data <- reactive({
    inFile <- input$file
    read.csv(inFile$datapath)
  })
  
  observe({
    if(!is.null(input$file)){
      transactions <- read_data()
      colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
      
      # set types
      type_options <- transactions %>% select(type) %>% distinct(type)
      updateSelectInput(session, 'type', choices=as.list(c('All', type_options)))
    }
  })

  observe({
    if(!is.null(input$file)){
      transactions <- read_data()
      colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
      
      # set types
      type_options <- transactions %>% select(type) %>% distinct(type)
      updateSelectInput(session, 'breakdown_type', choices=as.list(c('All', type_options)))
    }
  })
  
  observe({
    if(!is.null(input$file)){
      transactions <- read_data()
      colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
      
      # set subtypes of selected type
      subtype_options <- transactions %>% filter(type == input$type) %>% select(subtype) %>% distinct(subtype)
      updateSelectInput(session, 'subtype', choices=as.list(c('----', subtype_options)))
    }
  })
  
  observe({
    if(!is.null(input$file)){
      transactions <- read_data()
      colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
      
      # set subtypes of selected type
      transactions$date <- as.Date(transactions$date, format='%m/%d/%Y')
      transactions <- transactions %>% mutate(month = format(date, '%m'))
      transactions <- transactions %>%
        mutate(year = ifelse(month == '12', paste(format(date, '%Y'), '-', as.numeric(format(date, '%Y')) + 1),
                             ifelse(month == '01' | month == '02', paste(as.numeric(format(date, '%Y')) - 1, '-', format(date, '%Y')), format(date, '%Y'))
        ))
      transactions <- transactions %>%
        mutate(quarter = ifelse(month == '09' | month == '10' | month == '11', '3Fall',
                                ifelse(month == '12'| month == '01' | month == '02', '4Winter',
                                       ifelse(month == '03' | month == '04' | month == '05', '0Spring',
                                              ifelse(month == '06' | month == '07' | month == '08', '1Summer', 'Error')
                                       )
                                )
        ))
      transactions <- transactions %>% mutate(year_quarter = paste(substring(quarter, 2), year))
      date_options <- transactions %>% arrange(year, month) %>% select(year_quarter) %>% distinct(year_quarter)
      updateSelectInput(session, 'date', choices=as.list(c('----', date_options)))
    }
  })
  
  output$trendPlot <- renderPlotly({
    if(is.null(input$file)) {
      return(NULL)     
    } else {
      transactions <- read_data()
      
      # clean data
      colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
      transactions$date <- as.Date(transactions$date, format='%m/%d/%Y')
      transactions <- transactions %>% mutate(month = format(date, '%m'))
      transactions <- transactions %>%
        mutate(year = ifelse(month == '12', paste(format(date, '%Y'), '-', as.numeric(format(date, '%Y')) + 1),
                             ifelse(month == '01' | month == '02', paste(as.numeric(format(date, '%Y')) - 1, '-', format(date, '%Y')), format(date, '%Y'))
        ))
      transactions <- transactions %>%
        mutate(quarter = ifelse(month == '09' | month == '10' | month == '11', '3Fall',
                                ifelse(month == '12'| month == '01' | month == '02', '4Winter',
                                       ifelse(month == '03' | month == '04' | month == '05', '0Spring',
                                              ifelse(month == '06' | month == '07' | month == '08', '1Summer', 'Error')
                                       )
                                )
        ))
      transactions <- transactions %>% select(-month)
      
      # template with all the quarters
      template <- transactions %>%
        group_by(year, quarter) %>%
        summarize(amount = sum(amount)) %>%
        mutate(year_quarter = paste(substring(quarter, 2), year)) %>%
        mutate(amount = 0)
    }
    
    # filter
    if(input$type != 'All') {
      filtered_transactions <- transactions %>% filter(type == input$type)
      if(input$subtype != '----') {
        filtered_transactions <- filtered_transactions %>% filter(subtype == input$subtype)
      }
    } else {
      filtered_transactions <- transactions
    }
    
    # total earnings
    earnings <- filtered_transactions %>%
      filter(amount >= 0) %>%
      group_by(year, quarter) %>%
      summarize(amount = sum(amount)) %>%
      mutate(year_quarter = paste(substring(quarter, 2), year))
    earnings <- merge(earnings, template, by = c('year_quarter', 'year', 'quarter'), all=TRUE)
    earnings$amount.x[ is.na(earnings$amount.x) ] <- earnings$amount.y[ is.na(earnings$amount.x) ]
    earnings <- earnings %>% rename(amount = amount.x) %>% select(-amount.y) %>% arrange(year, quarter)

    # total losses
    losses <- filtered_transactions %>%
      filter(amount <= 0) %>%
      group_by(year, quarter) %>%
      summarize(amount = sum(amount)) %>%
      mutate(amount = amount * -1) %>%
      mutate(year_quarter = paste(substring(quarter, 2), year))
    losses <- merge(losses, template, by = c('year_quarter', 'year', 'quarter'), all=TRUE)
    losses$amount.x[ is.na(losses$amount.x) ] <- losses$amount.y[ is.na(losses$amount.x) ]
    losses <- losses %>% rename(amount = amount.x) %>% select(-amount.y) %>% arrange(year, quarter)
    
    # add difference columns
    earnings$difference <- earnings$amount - losses$amount
    losses$difference <- earnings$amount - losses$amount 
    
    # sort for plot labels
    earnings$year_quarter <- factor(earnings$year_quarter, levels = earnings$year_quarter[order(earnings$year, earnings$quarter)])
    losses$year_quarter <- factor(losses$year_quarter, levels = losses$year_quarter[order(losses$year, losses$quarter)])
    
    # plot
    plot_ly() %>%
      layout(xaxis=list(title='Quarter'), yaxis=list(title='Amount')) %>%
      add_trace(data=earnings, x = ~year_quarter, y = ~amount, type='scatter', mode='line+markers', name='Income',
                hoverinfo='text', text = ~paste0('Income: $', format(amount, big.mark=','),
                                                    ifelse(difference < 0,
                                                        paste0('</br>Loss: ', format(sub('-', '-$', difference), big.mark=',')),
                                                        paste0('</br>Gain: $', format(difference, big.mark=','))
                                                    )
                                                 )
                ) %>%
      add_trace(data=losses, x = ~year_quarter, y = ~amount, type='scatter', mode='line+markers', name='Expenses',
                hoverinfo='text', text = ~paste0('Expense: $', format(amount, big.mark=','),
                                                    ifelse(difference < 0,
                                                        paste0('</br>Loss: ', format(sub('-', '-$', difference), big.mark=',')),
                                                        paste0('</br>Gain: $', format(difference, big.mark=','))
                                                    )
                                                 )
                )
  })
  
  output$earningsBreakdown <- renderPlotly({
    if(is.null(input$file)) {
      return(NULL)     
    } else {
      transactions <- read_data()
      
      # clean data
      colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
      transactions$date <- as.Date(transactions$date, format='%m/%d/%Y')
      transactions <- transactions %>% mutate(month = format(date, '%m'))
      transactions <- transactions %>%
        mutate(year = ifelse(month == '12', paste(format(date, '%Y'), '-', as.numeric(format(date, '%Y')) + 1),
                             ifelse(month == '01' | month == '02', paste(as.numeric(format(date, '%Y')) - 1, '-', format(date, '%Y')), format(date, '%Y'))
        ))
      transactions <- transactions %>%
        mutate(quarter = ifelse(month == '09' | month == '10' | month == '11', '3Fall',
                                ifelse(month == '12'| month == '01' | month == '02', '4Winter',
                                       ifelse(month == '03' | month == '04' | month == '05', '0Spring',
                                              ifelse(month == '06' | month == '07' | month == '08', '1Summer', 'Error')
                                       )
                                )
        ))
      transactions <- transactions %>% select(-month)
      
      # get subtypes of current type
      subtype_options <- transactions %>% filter(type == input$breakdown_type, subtype != '') %>% select(subtype) %>% distinct(subtype)
    }
    
    
    # filter
    if(input$date != '----' && input$breakdown_type == 'All') {
      earnings <- transactions %>%
        filter(amount >= 0) %>%
        mutate(year_quarter = paste(substring(quarter, 2), year)) %>%
        filter(year_quarter == input$date) %>%
        group_by(type) %>%
        summarize(amount = sum(amount))
      
      plot_ly(data=earnings, labels = ~type, values = ~amount, type='pie') %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    } else if(input$date != '----' && nrow(subtype_options) != 0) {
      earnings <- transactions %>%
        filter(amount >= 0, type == input$breakdown_type) %>%
        mutate(year_quarter = paste(substring(quarter, 2), year)) %>%
        filter(year_quarter == input$date) %>%
        group_by(subtype) %>%
        summarize(amount = sum(amount))
        
        plot_ly(data=earnings, labels = ~subtype, values = ~amount, type='pie') %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    } else {
      plot_ly() %>%
        layout(title = 'No data to be shown.',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
})


