#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library("shiny")
library("shinyWidgets")
library("bslib")
library("shinyjs")
library("askgpt")
library("TheOpenAIR")

ui <- page_navbar(
  title = "Ask AI with R",
  theme = bs_theme(bootswatch = "minty"),
  window_title = "Ask AI with R",
  nav_panel("Chat", 
            textInput(inputId = "key", label = "Set your OpenAI API key:", value = Sys.getenv("OPENAI_KEY"), width = 1000, placeholder = "If you don' have one, then you can create here: https://platform.openai.com/api-keys"),
            selectInput("package", "Choose one R Package:", c("askgpt", "TheOpenAIR"), selected = "askgpt"),
            textAreaInput(inputId = "question", label = "Write here your question:", value = "", width = 1000, height = 200),
            actionButton("ask", "Answer me!"),
            verbatimTextOutput("answer") #,
            # downloadButton("downloadData", "Download")
  )
  
)


server <- function(input, output, session) {
  
  data <- data.frame()
  
  output$package <- renderPrint({
    
    package <- get(input$package)
    
  })
  
  output$key <- renderPrint({
    
    key <- get(input$key)
    
  })
  
  output$answer <- renderText({
    
    data
    
  })
  
  
  observeEvent(input$ask, {
    
    req(input$key)
    req(input$question)
    
    if (input$package == "askgpt"){
      
      tryCatch({
      
        login(api_key = input$key)
        instructions <- askgpt(prompt = input$question, return_answer = TRUE)
        instructions <- capture.output(cat(instructions))
        output$answer <- renderPrint({ writeLines(noquote(paste(instructions, sep = "\n")))  })
      
      },
      error = function(error_message){

        showModal(modalDialog(
          title = "Error!",
          "Please, set your correct OpenAI API key, which you can create here: https://platform.openai.com/api-keys",
          footer = modalButton("Ok"),
          fade = TRUE
        ))


      })
      
    }else if (input$package == "TheOpenAIR"){
      
      tryCatch({
        
        openai_api_key(input$key)
        instructions <- chat(input$question, output = "message")
        instructions <- capture.output(cat(instructions))
        output$answer <- renderPrint({ writeLines(noquote(paste(instructions, sep = "\n")))  })
        
      },
      error = function(error_message){
        
        showModal(modalDialog(
          title = "Error!",
          "Please, set your correct OpenAI API key, which you can create here: https://platform.openai.com/api-keys",
          footer = modalButton("Ok"),
          fade = TRUE
        ))
        
      })
      
    }
    
  })
  
  
  output$downloadData <- downloadHandler(
    
    # makeReactiveBinding("Data"),
    
    filename = function() {
      paste("data-", Sys.time(), ".csv", sep = "")
    },
    content = function(file) {
      print(input$answer)
      write.csv(input$answer, file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)