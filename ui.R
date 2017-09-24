# import libraries
library(shiny)
library(dplyr)
library(plotly)
library(rio)
library(lucr)

# shiny UI
shinyUI(
  fluidPage(
    h1('UWWRC Budget Visualization'),
    
    fileInput('file', 'Transaction History File (.csv)', accept = c('.csv')),
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput('type', 'Type', choices=list('All')),
        selectInput('subtype', 'Subtype', choices=list('----'))
      ),
      
      mainPanel(plotlyOutput('plot'))
    )
  )
)