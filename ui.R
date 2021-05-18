library(shiny)  

shinyUI(fluidPage(
    titlePanel("Next word predition model based on n-gram"),
    sidebarLayout(
        sidebarPanel(
            h2("Introduction"),
            h3("This app is used to predict the next word of text you enter"),
            h2("Instruction"),
            h3("Enter the text in the box below and hit submit button"),
            h2("Type your text:"),
            textInput("text1", "Ex: I will go to the"),
            submitButton("Submit")
        ),
        mainPanel(
            h3("The next word is"),
            textOutput("nextword")
        )
    )
))