#'-----------------------------------------------------------------------------
#'                              NEXT WORD PREDICTOR APP
#'
#'Input: type in a word or a sentence
#'Output: next word will be predicted
#'How prediction work?: it uses a simple backoff model searching next word from
#'fivegrams to unigrams depending on sentence length. Ngrams were previously 
#'built using a corpora of english text and "quanteda" library for text mining. 
#'
#'This is a Shiny web application. You can run the application by clicking
#' the 'Run App' button above.
#' 
#'Important: make sure to have in the same folder:
#'1) a data folder containing ngrams files.
#'2) global.R with main functions used for prediction and code to import ngrams
#'
#'Enjoy!
#'-----------------------------------------------------------------------------



# Load required libraries -----------------------------------------------------
library(shiny)
library(shinythemes)
library(stringr)


# Source ngrams and prediction functions --------------------------------------
source("./global.R")


# Shiny App -------------------------------------------------------------------


# Define UI
ui <- fluidPage(
  theme = shinytheme("slate"),
   
   # # Application title
   # titlePanel("Next Word Predictor"),
   # 
   # # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #       sliderInput("bins",
   #                   "Number of bins:",
   #                   min = 1,
   #                   max = 50,
   #                   value = 30),
   #       textInput("text","label", "enter your sentence")
   #    ),
   #    
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #       plotOutput("distPlot"),
   #       verbatimTextOutput("value")
   #    )
   # )
  
  fluidRow(
    # App title
    strong(h1("NEXT WORD PREDICTOR")),
    em("Data Science Capstone - JHU Coursera"),
    p(em("by Marco Pasin")),
    align="center"
    ),
  
  br(),
  br(),
  
  fluidRow(
    column(3),
    column(6,
           tags$div(textInput("text", 
                              label = h3("Type a word or sentence"),
                              value = "")),
           br(),
           br(),
           h3("Next word predicted",style = "color:blue"),
           verbatimTextOutput("value" ),
           align="center"
           
           ),
    column(3)
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
   
   output$value <- renderText({predict_next_word(input$text)})
}





# Put together server and ui and run the app
shinyApp(ui = ui, server = server)

# To deploy it on shinyapps.io:
# from your working directory
# rsconnect::deployApp(appTitle = "Next Word Predictor")