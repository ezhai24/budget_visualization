read.data <- function(inFile, session) {
  if(!is.null(inFile)) {
    # read file
    transactions <- read.csv(inFile$datapath)
    
    # clean data
    colnames(transactions) <- c('date', 'desc', 'amount', 'type', 'subtype')
    
    transactions <- transactions %>%
      # format date as date object
      mutate(date = as.Date(transactions$date, format='%m/%d/%Y')) %>%
      
      # create column for month
      mutate(month = format(date, '%m')) %>%
      
      # create column for year
      mutate(year = ifelse(month == '12', paste(format(date, '%Y'), '-', as.numeric(format(date, '%Y')) + 1),
                           ifelse(month == '01' | month == '02', paste(as.numeric(format(date, '%Y')) - 1, '-', format(date, '%Y')), format(date, '%Y'))
      )) %>%
      
      # create column for quarter
      mutate(quarter = ifelse(month == '09' | month == '10' | month == '11', '3Fall',
                              ifelse(month == '12'| month == '01' | month == '02', '4Winter',
                                     ifelse(month == '03' | month == '04' | month == '05', '0Spring',
                                            ifelse(month == '06' | month == '07' | month == '08', '1Summer', 'Error')
                                     )
                              )
      )) %>%
      
      # remove the month column
      select(-month)
    
    return(transactions)
  }
}

shinyServer(function(input, output, session) {
  
  currentData <- reactive({ read.data(input$file, session) })

  # set types
  observe({
    transactions <- currentData()

    if(!is.null(transactions)) {
      type_options <- transactions %>% distinct(type)
      updateSelectInput(session, 'type', choices=c('All', as.vector(type_options$type)))
    }
  })
  
  # set subtypes
  observe({
    transactions <- currentData()
    
    if(input$type != 'All') {
      subtype_options <- transactions %>% filter(type == input$type) %>% distinct(subtype)
      updateSelectInput(session, 'subtype', choices = c('----', as.vector(subtype_options$subtype)))
    }
  })
  
  # render trend plot
  output$trendPlot <- renderPlotly({
    transactions <- currentData()
    
    # check for an input file
    if(is.null(input$file)) {
      return(NULL)     
    }
    
    # template with all the quarters
    template <- transactions %>%
      group_by(year, quarter) %>%
      summarize(amount = sum(amount)) %>%
      mutate(year_quarter = paste(substring(quarter, 2), year)) %>%
      mutate(amount = 0)
    
    # filter by type
    if(input$type != 'All') {
      transactions <- transactions %>% filter(type == input$type)
      
      # filter by subtype
      if(input$subtype != '----') {
        transactions <- transactions %>% filter(subtype == input$subtype)
      }
    }
    
    # total earnings
    earnings <- transactions %>%
      filter(amount >= 0) %>%
      group_by(year, quarter) %>%
      summarize(amount = sum(amount)) %>%
      mutate(year_quarter = paste(substring(quarter, 2), year))
    earnings <- merge(earnings, template, by = c('year_quarter', 'year', 'quarter'), all=TRUE)
    earnings$amount.x[ is.na(earnings$amount.x) ] <- earnings$amount.y[ is.na(earnings$amount.x) ]
    earnings <- earnings %>% rename(amount = amount.x) %>% select(-amount.y) %>% arrange(year, quarter)
    
    # total losses
    losses <- transactions %>%
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
                                                        paste0('<br>Loss: ', format(sub('-', '-$', difference), big.mark=',')),
                                                        paste0('<br>Gain: $', format(difference, big.mark=','))
                                                 )
                )
      ) %>%
      add_trace(data=losses, x = ~year_quarter, y = ~amount, type='scatter', mode='line+markers', name='Expenses',
                hoverinfo='text', text = ~paste0('Expense: $', format(amount, big.mark=','),
                                                 ifelse(difference < 0,
                                                        paste0('<br>Loss: ', format(sub('-', '-$', difference), big.mark=',')),
                                                        paste0('<br>Gain: $', format(difference, big.mark=','))
                                                 )
                )
      )
  })
  
})