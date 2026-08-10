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
library(pals)
library(wordcloud2)
library(tidytext)

#find newest data set
data_files = list.files(path = 'data', pattern = 'ebola-news_\\d+.csv', full.names = T)
data_dates = sapply(data_files, function(x) file.info(x)$mtime)
data_path = data_files[which.max(data_dates)]

#load_data
data <- read.csv(data_path)
ind = c(2,3,6)
col_names = colnames(data)[ind]
col_names = c("None", col_names)

# make sure data is in correct date format
data$Days = as.Date(data$Date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
# make another column by month
data$Months = floor_date(data$Days, unit = "months")
data$Months_select = format(data$Months, format = "%b %Y")
# make another column by week
data$Weeks = floor_date(data$Days, unit = "weeks")

# empty countries to NA
#data$Country2[data$Country2 == ""] = NA
#data$Country3[data$Country3 == ""] = NA

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
            
            layout_sidebar(width = 600,
                           
                           sidebar = sidebar(open = "always",
                                             shiny::p("Pick options to filter the data and display temporal trends:"),
                                             uiOutput("select_timescale"),
                                             uiOutput("colour_by"),
                                             conditionalPanel(
                                               condition = "input.colour_by != 'None'",
                                               checkboxInput("stack", "Stack bars", value = TRUE, width = NULL)
                                               ),
                                             shiny::p("Filter"),
                                             uiOutput("select_pub"),
                                             uiOutput("select_country"),
                                             uiOutput("select_topic"),
                                             uiOutput("select_date"),
                                             
                                             selectInput("state", "Choose a state:",
                                                         list(`East Coast` = c("NY", "NJ", "CT"),
                                                              `West Coast` = c("WA", "OR", "CA"),
                                                              `Midwest` = c("MN", "WI", "IA")), multiple = T)
                                             ),
                                             
                           plotOutput("plotTrends"),
                           
                           )
            
            
            ), 
  
  nav_panel("Keywords",
            shiny::p((tags$h3("Headline keywords"))),
            
            layout_sidebar(
              width = 600,
              
              
              sidebar = sidebar(open = "always",
                                shiny::p(tags$h6("Filter headline keywords")),
                                
                                shiny::tags$small(tags$em("**Note that word size is proportional to the square root of
                                         the word count in the data to preserve readibility of low count words**")),
                                
                                uiOutput("select_pub_cloud"),
                                
                                uiOutput("select_country_cloud"),
                                
                                uiOutput("select_topic_cloud"),
                                
                                uiOutput("select_date_cloud"),
                                
                                ),
              
              uiOutput("displayWordCloud"),
              
            )
            
          ), 
  



)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #simple outputs to base ui inputs on what is in the data
  output$select_pub <- renderUI({
    selectInput("publication", 
                label = "Published in:",
                choices = unique(data$Publication),
                            selected = NULL,
                            multiple = T
                )
    })
  
  output$select_country <- renderUI({
    selectInput("country", 
                label = "Mentions country/region:",
                choices = unique(c(data$Country, data$Country2[!is.na(data$Country2)], data$Country3[!is.na(data$Country3)])), selected = NULL, multiple = T
    )
  })
  
  output$select_topic <- renderUI({
    selectInput("topic", 
                label = "About topic:",
                choices = unique(data$Topic), 
                selected = NULL, multiple = T
    )
  })
  
  output$select_date <- renderUI({
    selectInput("date", 
                label = "In month:",
                choices = unique(data$Months_select), selected = NULL, multiple = T
    )
  })
  
  output$select_timescale <- renderUI({
    selectInput("timescale", 
                label = "Time scale:",
                choices = c("Days", "Weeks", "Months"),
                selected = "Weeks"
    )
  })
  
  output$colour_by <- renderUI({
    selectInput("colour_by", 
                label = "Colour by:",
                choices = col_names,
                selected = "None"
    )
  })
  
  colour <- reactive({input$colour_by})
  stack <- reactive({input$stack})
  
  plot_dat <- reactive({
    df <- data %>%
      filter((is.null(input$publication) | Publication %in% input$publication) &
               (is.null(input$country) | Country %in% input$country | Country2 %in% input$country | Country3 %in% input$country) &
               (is.null(input$topic) | Topic %in% input$topic) &
               (is.null(input$date) | Months_select %in% input$date))
    df
  })
  
  output$plotTrends <- renderPlot({
    
    if(colour() == "None"){
      # don't colour plot
      g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]])) + geom_bar(fill = "grey90")
    } else if(colour() == "Country"){
      a = interaction(plot_dat()$Country, plot_dat()$Country2, plot_dat()$Country3)
      names = levels(a)
      names = gsub("[.]"," ", names)
      g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]], fill = a)) + scale_fill_manual(values=as.vector(alphabet2(24)), labels = names)
    } else {
      # colour plot
      g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]], fill = .data[[input$colour_by]])) + scale_fill_manual(values=as.vector(alphabet2(24)))
    }
    
    if(!stack()){
      g <- g + geom_bar(position = position_dodge(preserve = "single")) 
    } else{
      g <- g + geom_bar() 
    }
    g <- g + theme_bw(base_size = 12) + ylab("Number of articles")
    g
  })
  
  #---- wordcloud server functions
  
  #simple outputs to base ui inputs on what is in the data for key words
  output$select_pub_cloud <- renderUI({
    selectInput("publication_cloud", 
                label = "Published in:",
                choices = unique(data$Publication),
                multiple = T
    )
  })
  
  output$select_country_cloud <- renderUI({
    selectInput("country_cloud", 
                label = "Mentions country:",
                choices = c("DRC", "Uganda", "USA", "UK"),
                multiple = T
    )
  })
  
  output$select_topic_cloud <- renderUI({
    selectInput("topic_cloud", 
                label = "About topic:",
                choices = c("Epidemic", "Funding/aid", "Protests", "Repatriation/Importation", "Travel restrictions", "World cup", "Therapeutics/diagnostics"),
                multiple = T
    )
  })
  
  output$select_date_cloud <- renderUI({
    selectInput("date_cloud", 
                label = "In month:",
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
  
  #colour_cloud <- reactive({input$colour_by_cloud})
  
  cloud_dat <- reactive({
    df <- data %>%
      filter((is.null(input$publication_cloud) | Publication %in% input$publication_cloud) &
               (is.null(input$country_cloud) | Country %in% input$country_cloud | Country2 %in% input$country_cloud | Country3 %in% input$country_cloud) &
               (is.null(input$topic_cloud) | Topic %in% input$topic_cloud) &
               (is.null(input$date_cloud) | Months_select %in% input$date_cloud))
    
    headlines <- data.frame(text = df$Headline)
    
    headline_words <- headlines %>% unnest_tokens(word, text)
    
    word_freq <- headline_words |>
      anti_join(stop_words, by = "word") |>
      count(word, sort = TRUE)
  
    word_freq = word_freq[word_freq$n>1,]
    
    headline_phrase <- headlines %>% unnest_tokens(twoword, text, token = "ngrams", n = 2)
    
    twoword_freq <- headline_phrase %>%
      separate(twoword, into = c("first","second"), sep = " ", remove = FALSE) %>%
      anti_join(stop_words, by = c("first" = "word")) %>%
      anti_join(stop_words, by = c("second" = "word")) %>%
      filter(str_detect(first, "[a-z]") &
               str_detect(second, "[a-z]")) %>% count(twoword, sort = TRUE)
    
    twoword_freq$n = sqrt(twoword_freq$n)
    
    twoword_freq

  })
  
  output$wordCloud <- renderWordcloud2({
    colorVec = rev(coolwarm(nrow(cloud_dat())))
    wordcloud2(cloud_dat(), color = colorVec, fontFamily = "courier", rotateRatio = 0.2, size = 0.4)
  })
  
  output$displayWordCloud <- renderUI({
    wordcloud2Output("wordCloud", height = 600)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
