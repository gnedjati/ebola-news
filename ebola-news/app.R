#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(bslib)

# Define UI for news application with 
ui <- page_navbar(
  
  theme = bslib::bs_theme(version=5,bootswatch="lux"),
  title = "Tracking UK news about the current BVD outbreak", 
  id = "page",
  
  nav_panel("About",
            shiny::p(tags$h3("About the project")),
            
            shiny::p("More information about the project")
            
            
            ),
  nav_panel("Temporal trends", 
            
            shiny::p(tags$h3("Temporal trends")),
            
            shiny::p("Pick options to filter or facet the data and display temporal trends"),
            ), 
  
  nav_panel("Keywords",
            shiny::p((tags$h3("Headline keywords"))),
            
            shiny::p("Pick options to filter or facet the data and keyword word clouds"),
            
            ), 
  



)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #find newest data set
  data_files = list.files(path = 'data', pattern = 'ebola-news_\\d+.csv', full.names = T)
  data_dates = sapply(data_files, function(x) file.info(x)$mtime)
  data_path = data_files[which.max(data_dates)]
   
  #load_data
  data <- read.csv(data_path)
  
  # make sure data is in correct date format
  data$Date = as.Date(data$Date, format = "%d/%m/%y")
  # make another column by month
  data$Month = floor_date(data$Date, unit = "months")

}

# Run the application 
shinyApp(ui = ui, server = server)
