library(shiny)  

shinyUI(fluidPage(
    titlePanel("Next word prediction model based on n-gram"),
    sidebarLayout(
        sidebarPanel(
            h4(strong("Instruction")),
            h5("This app is used to predict the next word of text you enter."),
            h5("Enter the text in the box below, then hit enter or the submit button to see the result."),
            textInput("text1", label= "Please type your text here:"),
            h6(em("Note: program will ignore numbers and special characters")),
            submitButton("Submit")
        ),
        mainPanel(
            tabsetPanel(type="tabs",
                        tabPanel(strong("Prediction"), h5(strong("The predicted next word is:")), pre(textOutput("nextword"))),
                        tabPanel(strong("Algorithm"), 
                                 h5("The algorithm of prediction model is built based on Kneser Ney Smoothing."),
                                 h5("The overal formular for Kneser Ney Smoothing method can be found on this link https://en.wikipedia.org/wiki/Kneser%E2%80%93Ney_smoothing"),
                                 h5("This is a method primarily used to calculate the probability distribution of n-grams in a document based on their histories. The next word is suggested based on not only it's probability but also the words preceding it."),
                                 h5("We shall build 3 functions. The first one randomly return one of five words having highest probability. The second one return the word with highest probability to go with one preceding word. The last one return the highest frequent word to appear after two preceding words.Then we build the function to predict the next word when we type a sequence of words. It will take the guess from trigram first. If there is no word found, it will work in bigram, then unigram.")
                                 )
            )
        )
    )
))
