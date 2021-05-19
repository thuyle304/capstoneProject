library(shiny)  

shinyUI(fluidPage(
    titlePanel("Next word prediction model based on n-gram"),
    sidebarLayout(
        sidebarPanel(
            h4("Introduction"),
            h6("This app is used to predict the next word of text you enter"),
            h4("Instruction"),
            h6("Enter the text in the box below and hit submit button"),
            h6("Type your text:"),
            textInput("text1", label= "Please type your text here:"),
            h6(em("Note: program will ignore numbers and special characters")),
            submitButton("Submit")
        ),
        mainPanel(
            h3("The next word is"),
            textOutput("nextword")
        )
    )
))