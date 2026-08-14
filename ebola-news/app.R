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

##think this is probably causing the deployment issue as when the app is build, the data sets have the same time stamps
##find newest data  
# data_files = list.files(path = 'data', pattern = 'ebola-news_\\d+.csv', full.names = T)
# data_dates = sapply(data_files, function(x) file.info(x)$mtime)
# data_path = data_files[which.max(data_dates)]
data_path = file.path("data","ebola-news_090826.csv")

#case data
case_path = file.path("data","cumulative_confirmed_cases__daily.csv")

#load_data
data <- read.csv(data_path)
ind = c(2,3,6)
col_names = colnames(data)[ind]
col_names = c("None", col_names)

# make sure data is in correct date format
data$Days = as.Date(data$Date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))
# make another column by month
data$Months = floor_date(data$Days, unit = "months")
data$Months_select = format(data$Months, format = "%B %Y")
# make another column by week
data$Weeks = floor_date(data$Days, unit = "weeks")

# load case data
case_data <- read.csv(case_path)
# make sure data is in correct date format
case_data$Days = as.Date(case_data$date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))


# Define UI for news application with 
ui <- page_navbar(
  
  theme = bslib::bs_theme(version=5,bootswatch="lux"),
  title = "Tracking UK news about the current BVD outbreak", 
  id = "page",
  
  nav_panel("About",
            shiny::p(tags$h3("About the project")),
            
            shiny::p("On 15th May 2026, Democratic Republic of Congo announced the country's 17th Ebola outbreak.
                    The outbreak began in the North East province of Ituri, and has since spread to North Kivu,
                    South Kivu, Tshopo and Haut Uele provinces."),
            
            shiny::p("This outbreak is caused by the less common Bundibugyo strain, for which there is no licensed
                     vaccine or therapeutics, leading to initial widespread media interest."),
            
            shiny::p("This site tracks UK media interest in the outbreak over time, focusing on national news media.
                     We currently track articles published on the Guardian, Independent, Sun and Daily Mail online sites.
                     At the moment we do not track paywalled sites including the Times, Telegraph and Financial Times."),
            
            shiny::p("Articles are sourced either by searching a news website for the keyword or tag 'ebola' from 15th May 2026, or in cases
                     where search results are truncated by date, using a site-specific Google search, e.g:"),
            
            shiny::p(tags$code("site:www.independent.co.uk ebola",)),
            
            shiny::p("and using the custom date range to search only for articles published from 15th May 2026 onwards. We excluded
                     articles that were not credited to named authors (i.e. articles from newswire services such as Associated Press,
                     PA Media, etc.), a limited number of paywalled premium content articles and undated video reports. For each article
                     we extracted the date, publication, country/region mentioned in the article (up to 3), topic, link and headline.
                     Data were entered into a Google spreadsheet, which was then downloaded as a csv file and imported into R for analysis.")
                    
            
            
            ),
  nav_panel("Temporal trends", 
            
            shiny::p(tags$h3("Temporal trends")),
            
            layout_sidebar(width = 600,
                           
                           sidebar = sidebar(open = "always",
                                             shiny::p(tags$h6("Explore articles over time")),
                                             uiOutput("select_timescale"),
                                             checkboxInput("casedata", "Show case data", value = FALSE, width = NULL),
                                             uiOutput("colour_by"),
                                             conditionalPanel(
                                               condition = "input.colour_by != 'None'",
                                               checkboxInput("stack", "Stack bars", value = TRUE, width = NULL)
                                               ),
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
              width = 600,
              
              
              sidebar = sidebar(open = "always",
                                shiny::p(tags$h6("Explore headline keywords")),
                                
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
    req(input$timescale)
    if('colour_by'%in%names(input) && input$colour_by != "None"){
      if(input$colour_by == "Country"){
        a = interaction(plot_dat()$Country, plot_dat()$Country2, plot_dat()$Country3, sep = " ")
        names = levels(a)
        #names = gsub("[.]"," ", names)
        g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]], fill = a)) + 
          scale_fill_manual("Country/Region", values=as.vector(alphabet2(24)))
      } else{
        g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]], fill = .data[[input$colour_by]])) + 
          scale_fill_manual(values=as.vector(alphabet2(24)))
      }
    } else {
      # don't colour plot
      g <- ggplot(data = plot_dat(), aes(x = .data[[input$timescale]])) #+ geom_bar() it is added later
    }
    
    if('stack'%in%names(input) && input$stack){
      g <- g + geom_bar()
    } else{
      g <- g + geom_bar(position = position_dodge(preserve = "single")) 
    }
    
    #make subtitle
    if(is.null(input$publication)){
      pubstring = ""
    } else{
      pubstring = paste("published in the",paste(input$publication, collapse = " and "))
    }
    
    if(is.null(input$date)){
      datestring = ""
    } else{
      datestring = paste("in",paste(input$date, collapse = " and "))
    }
    
    if(is.null(input$topic)){
      topicstring = ""
    } else{
      topics = str_to_lower(input$topic)
      topics = case_match(topics,
                 "other" ~ "other topics",
                 "ebola virus" ~ "the ebola virus",
                 "epidemic" ~ "the epidemic",
                 "world cup" ~ "the World Cup",
                 "environment" ~ "the environment",
                 .default = topics)
      topicstring = paste("about",paste(topics, collapse = " and "))
    }
    
    if(is.null(input$country)){
      countrystring = ""
    } else{
      conj = ifelse(topicstring == "", "about", "in")
      countrystring = paste(conj,paste(input$country, collapse = " and "))
    }
    subtitle = paste(pubstring, datestring, topicstring, countrystring)
    
    if('casedata' %in% names(input) && input$casedata){
      if(input$timescale == "Days") {
        coeff = 200
      } else if (input$timescale == "Weeks") {
        coeff = 50
      } else {
        coeff = 30
      }
      g <- g + geom_line(data = case_data, aes(x = Days, y = national_cumulative_confirmed_cases/coeff, color = "black")) +
        scale_y_continuous(name = "Number of articles", sec.axis = sec_axis(~.*coeff, name = "Cumulative confirmed cases"))
    }
    
    g <- g + theme_bw(base_size = 15) + labs(title = "Articles over time", subtitle = subtitle)
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
