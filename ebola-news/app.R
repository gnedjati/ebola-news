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
library(viridis)
library(wordcloud)
library(tidytext)

#find newest data set
data_files = list.files(path = 'data', pattern = 'ebola-news_\\d+.csv', full.names = T)
data_dates = sapply(data_files, function(x) file.info(x)$mtime)
data_path = data_files[which.max(data_dates)]

#load_data
data <- read.csv(data_path)

col_names = colnames(data)[2:4]
col_names = c("", col_names)

# make sure data is in correct date format
data$Days = as.Date(data$Date, format = "%d/%m/%Y")
# make another column by month
data$Months = floor_date(data$Days, unit = "months")
data$Months_select = format(data$Months, format = "%b %Y")
# make another column by week
data$Weeks = floor_date(data$Days, unit = "weeks")

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
            
            layout_sidebar(width = 300,
                           
                           sidebar = sidebar(open = "always",
                                             shiny::p("Pick options to filter the data and display temporal trends:"),
                                             uiOutput("select_timescale"),
                                             uiOutput("colour_by"),
                                             conditionalPanel(
                                               condition = "input.colour_by.length > 0",
                                               checkboxInput("stagger", "Stagger bars:", value = FALSE, width = NULL)
                                               ),
                                             shiny::p("Filter by:"),
                                             uiOutput("select_pub"),
                                             uiOutput("select_country"),
                                             uiOutput("select_topic"),
                                             uiOutput("select_date"),
                                             ),
                                             
                           plotOutput("plotTrends"),
                           
                           )
            
            
            ), 
  
  nav_panel("Keywords",
            shiny::p((tags$h3("Headline keywords"))),
            
            layout_sidebar(
              width = 300,
              
              
              sidebar = sidebar(open = "always",
                                shiny::p("Pick options to filter or facet the data and display keyword clouds:"),
                                
                                uiOutput("select_pub_cloud"),
                                
                                uiOutput("select_country_cloud"),
                                
                                uiOutput("select_topic_cloud"),
                                
                                uiOutput("select_date_cloud"),
                                
                                #uiOutput("colour_by_cloud"),
                                
                                ),
              
              plotOutput("plot"),
              
            )
            
          ), 
  



)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #simple outputs to base ui inputs on what is in the data
  output$select_pub <- renderUI({
    selectInput("publication", 
                label = "Publication:",
                choices = unique(data$Publication),
                            selected = NULL,
                            multiple = T
                )
    })
  
  output$select_country <- renderUI({
    selectInput("country", 
                label = "Country:",
                choices = unique(data$Country), selected = NULL, multiple = T
    )
  })
  
  output$select_topic <- renderUI({
    selectInput("topic", 
                label = "Topic:",
                choices = unique(data$Topic), 
                selected = NULL, multiple = T
    )
  })
  
  output$select_date <- renderUI({
    selectInput("date", 
                label = "Date:",
                choices = unique(data$Months_select), selected = NULL, multiple = T
    )
  })
  
  output$select_timescale <- renderUI({
    selectInput("timescale", 
                label = "Time scale:",
                choices = c("Days", "Weeks", "Months")
    )
  })
  
  output$colour_by <- renderUI({
    selectInput("colour_by", 
                label = "Colour by:",
                choices = col_names,
                selected = NULL,
                multiple = F
    )
  })
  
  #simple outputs to base ui inputs on what is in the data for key words
  output$select_pub_cloud <- renderUI({
    selectInput("publication_cloud", 
                label = "Publication",
                choices = unique(data$Publication),
                multiple = T
    )
  })
  
  output$select_country_cloud <- renderUI({
    selectInput("country_cloud", 
                label = "Country",
                choices = c("DRC", "Uganda", "USA", "UK"),
                multiple = T
    )
  })
  
  output$select_topic_cloud <- renderUI({
    selectInput("topic_cloud", 
                label = "Topic",
                choices = c("Epidemic", "Funding/aid", "Protests", "Repatriation/importation", "Travel restrictions", "World cup", "Therapeutics/diagnostics"),
                multiple = T
    )
  })
  
  output$select_date_cloud <- renderUI({
    selectInput("date_cloud", 
                label = "Date",
                choices = unique(data$Months_select),
                multiple = T
    )
  })
  
  output$colour_by_cloud <- renderUI({
    selectInput("colour_by_cloud", 
                label = "Colour by:",
                choices = col_names,
                selected = NULL,
                multiple = F
    )
  })
  
  
  colour <- reactive({input$colour_by})
  
  plot_dat <- reactive({
    df <- data %>%
      filter((is.null(input$publication) | Publication %in% input$publication) &
               (is.null(input$country) | Country %in% input$country) &
               (is.null(input$topic) | Topic %in% input$topic) &
               (is.null(input$date) | Months_select %in% input$date))
    df
  })
  
  output$plotTrends <- renderPlot({
    
    if(input$colour_by == ""){
      # don't colour plot
      g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]])) + geom_bar()
    } else {
      # colour plot
      g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]], fill = .data[[input$colour_by]])) + scale_fill_viridis(discrete = T, option = "magma")
    }
    
    if(input$stagger){
      g <- g + geom_bar(position = position_dodge(preserve = "single")) 
    } else{
      g <- g + geom_bar() 
    }
    g
  })
  
  #colour_cloud <- reactive({input$colour_by_cloud})
  
  cloud_dat <- reactive({
    df <- data %>%
      filter((is.null(input$publication_cloud) | Publication %in% input$publication_cloud) &
               (is.null(input$country_cloud) | Country %in% input$country_cloud) &
               (is.null(input$topic_cloud) | Topic %in% input$topic_cloud) &
               (is.null(input$date_cloud) | Months_select %in% input$date_cloud))
    
    headlines <- data.frame(text = df$Headline)
    tidy_headlines <- headlines %>% unnest_tokens(word, text)
    word_freq <- tidy_headlines |>
      anti_join(stop_words, by = "word") |>
      count(word, sort = TRUE)
    
    word_freq

  })
  

  
  output$plot <- renderPlot({
    pal <- viridis(10)
    cloud_dat() %>% 
      with(wordcloud(word, n, random.order = FALSE, scale = c(10, 1), max.words = 700, col = pal, 
                     family = "mono", font = 2))
  })
  
  output$wordCloud <- renderPlot({
    headlines <- data.frame(text = cloud_dat()$Headline)
    tidy_headlines <- headlines %>% unnest_tokens(word, text)
    word_freq <- tidy_headlines |>
      anti_join(stop_words, by = "word") |>
      count(word, sort = TRUE)
    
    word_freq = filter(word_freq,n >1)
    
    
    set.seed(42)
    ggplot(word_freq, aes(label = word, size = n, colour = n)) +
      geom_text_wordcloud() +
      scale_size_area(max_size = 20) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
