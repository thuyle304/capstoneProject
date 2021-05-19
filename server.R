
library(shiny) 
# Define server logic required to draw a histogram
shinyServer(function(input, output){
    library(quanteda)  
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(data.table)
    library(stringr)
    source("Capstone4.R")
    
    output$nextword <- renderText({
        as.character(getWords(input$text1))})
})


