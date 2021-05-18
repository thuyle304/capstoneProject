
library(shiny) 
# Define server logic required to draw a histogram
shinyServer(function(input, output){
    library(quanteda)  
    library(dplyr)
    library(tidyr)
    library(data.table)
    library(stringr)
    unigram <- read.csv(file="unigram.csv")
    bigram <- read.csv(file = "bigram.csv")
    trigram <- read.csv(file = "trigram.csv")
    source("Capstone4.R")
    word <- reactive({
        nextword <- getWords(input$text1)
        return(nextword)
    })
    output$nextword <- renderText({word})
})


