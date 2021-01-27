##load library

library(shiny)
library(shinycssloaders)
library(wordcloud)
library(ggplot2)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(DT)

##simpan data hotels kedalam directory
setwd('D:/project_final')
hotels <- readRDS('data/hotels.rds')

#buka data yang telah discraping
source("scraper/scrapReview.R")
source("model-dan-dataset/modelNB.R")

#membuat ui
ui <- fluidPage(
  
  titlePanel("Sentiment Analysis of Hotel's from Yogyakarta"),
  
  sidebarLayout(
    
    sidebarPanel(
      uiOutput("selectHotel")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          helpText("Tihs app will display the sentiment classification of hotels's user review from TripAdvisor
                   website. The sentiment will be split into Positive Sentiment and Negative Sentiment.")
        ),
        tabPanel(
          "User Review & Sentiment Classification",
          fluidRow(
            box(
              title = "User Review",
              solidHeader = T,
              width = 12,
              collapsible = T,
              div(DT::dataTableOutput("table_review") %>% withSpinner(color="#1167b1"), style = "font-size: 70%;")
            ),
            box(title = "Sentiment Classification",
                solidHeader = T,
                width = 12,
                collapsible = T,
                plotOutput("plot") %>% withSpinner(color="#1167b1")
            )
          )
        ),
        tabPanel(
          "Wordcloud",
          fluidRow(
            box(title = "Wordcloud",
                solidHeader = T,
                width = 12,
                collapsible = T,
                plotOutput("wordcloud") %>% withSpinner(color="#1167b1")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  var <- reactive({
            setNames(hotels$link, hotels$name)

  })
  
  output$selectHotel <- renderUI({
    selectInput(inputId  =  "selectHotel", 
                label = "Pilih hotel",
                choices = var())
  })
  
  dataScrap <- reactive({
    result <- get_hotel_reviews(input$selectHotel, incProgress)
    return(result)
  })
  
  data_prediction <- reactive({
    withProgress({
      setProgress(message = "Predicting", value = 0)
      
      reviews <- dataScrap()$review
      incProgress(1/2)
      prediction <- get_prediction(reviews)
      incProgress(1/2)
    })
    prediction$reviewer <- dataScrap()$reviewer
    
    return(prediction)
  })
  
  output$total_review <- renderText({
    paste0("This hotel has ", nrow(dataScrap()), " review")
  })
  
  output$table_review <- renderDataTable(datatable({
    data_prediction()
  }))
  
  output$wordcloud <- renderPlot({
    data_corpus <- clean_data(dataScrap()$review)
    wordcloud(data_corpus, min.freq = 30, max.words = 50)
  })

  output$plot <- renderPlot({
    Classification <- data_prediction()$sentiment
    Total <- nrow(data_prediction())
    ggplot(data_prediction(), aes(x = Classification, y = Total, fill = Classification)) + geom_col()
  })
  
}

shinyApp(ui, server)
