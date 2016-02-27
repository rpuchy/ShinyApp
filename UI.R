#this is part of the shiny project this is UI.R
library(shiny) 
library(dygraphs)
stocks= c("MCO","MSFT","IBM","LNKD","TSLA","FB","TWTR","AMZN","AMD","INTC")
shinyUI(pageWithSidebar( 
     headerPanel("My Awesome Shiny App"), 
     sidebarPanel( 
           selectInput('inStocks', 'Stocks', stocks, multiple=TRUE, selectize=TRUE),
           dateInput("StartDate", " Start Date:", value = "2010-01-01"),
           dateInput("EndDate", "End Date:", value = "2016-01-01"),
           actionButton("goButton", "Chart!"), 
           br(),
           includeHTML("include.html")
       ), 
     mainPanel( 
       dygraphOutput("dygraph"),
       plotOutput("EffFrontier")
       ) 
   )) 
