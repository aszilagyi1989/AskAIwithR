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
library("chatAI4R")
library("gptr")
library("openai")
library("stringr")

ui <- page_navbar(
  title = "Ask AI with R",
  theme = bs_theme(bootswatch = "minty"),
  window_title = "Ask AI with R",
  navset_card_underline(id = "navset", 
                        
    nav_panel("Chat", 
              passwordInput(inputId = "key", label = "Set your OpenAI API key:", value = Sys.getenv("OPENAI_API_KEY"), width = 1000, placeholder = "If you don't have one, then you can create here: https://platform.openai.com/api-keys"),
              selectInput("package", "Choose one R Package:", c("TheOpenAIR", "chatAI4R", "gptr", "askgpt"), selected = "TheOpenAIR"), 
              textAreaInput(inputId = "question", label = "Write here your question:", value = "", width = 1000, height = 200),
              actionButton("ask", "Answer me!"),
              verbatimTextOutput("answer"),
              downloadButton("downloadData", "Download")
    ),
    nav_panel("Image", 
              passwordInput(inputId = "key2", label = "Set your OpenAI API key:", value = Sys.getenv("OPENAI_API_KEY"), width = 1000, placeholder = "If you don't have one, then you can create here: https://platform.openai.com/api-keys"),
              selectInput("size", "Choose size of the picture:", c("256x256", "512x512", "1024x1024"), selected = "256x256"), 
              textAreaInput(inputId = "depict", label = "What should the picture depict?", value = "", width = 1000, height = 100),
              actionButton("draw", "Draw me!"),
              imageOutput("image"),
    ),
    nav_panel("Gallery", 
              uiOutput("images"),
    )
    
  )
  
)


server <- function(input, output, session) {
  
  data <- reactiveVal()
  
  output$package <- renderPrint({
    
    package <- get(input$package)
    
  })
  
  output$key <- renderPrint({
    
    key <- get(input$key)
    
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
        
        if(length(data()) == 0)
          data(data.frame(input$package, input$question, instructions))
        else
          data(rbind(data(), data.frame(input$package, input$question, instructions)))
        isolate(data())
      
       },
       error = function(error_message){
           showModal(modalDialog(
             title = "Error!",
             as.character(error_message),
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

        if(length(data()) == 0)
          data(data.frame(input$package, input$question, instructions))
        else
          data(rbind(data(), data.frame(input$package, input$question, instructions)))
        isolate(data())
        
      },
      error = function(error_message){
        
        showModal(modalDialog(
          title = "Error!",
          as.character(error_message),
          footer = modalButton("Ok"),
          fade = TRUE
        ))
        
      })
      
    }else if (input$package == "chatAI4R"){
      
      tryCatch({
        
        instructions <- chat4R(input$question, temperature = 0, simple = TRUE, api_key = input$key)
        instructions <- capture.output(cat(instructions$content))
        output$answer <- renderPrint({ writeLines(noquote(paste(instructions, sep = "\n")))  })
        
        if(length(data()) == 0)
          data(data.frame(input$package, input$question, instructions))
        else
          data(rbind(data(), data.frame(input$package, input$question, instructions)))
        isolate(data())
        
      },
      error = function(error_message){
        
        showModal(modalDialog(
          title = "Error!",
          as.character(error_message),
          footer = modalButton("Ok"),
          fade = TRUE
        ))
        
      })
      
    }else if (input$package == "gptr"){
      
      tryCatch({
        
        instructions <- get_response(input$question, api_key = input$key, print_response = FALSE)
        instructions <- capture.output(cat(instructions[["choices"]][["message"]]$content))
        output$answer <- renderPrint({ writeLines(noquote(paste(instructions, sep = "\n")))  })
        
        if(length(data()) == 0)
          data(data.frame(input$package, input$question, instructions))
        else
          data(rbind(data(), data.frame(input$package, input$question, instructions)))
        isolate(data())
        
      },
      error = function(error_message){
        
        showModal(modalDialog(
          title = "Error!",
          as.character(error_message),
          footer = modalButton("Ok"),
          fade = TRUE
        ))
        
      })
      
    }
    
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      paste("data-", Sys.time(), ".csv", sep = "")
      
    },
    
    content = function(file) {

      write.table(data(), file, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ";")
      
    }
    
  )
  
  
  observeEvent(input$draw, {
    
    req(input$key2)
    req(input$size)
    req(input$depict)
    
    tryCatch({
        
      output$image <- renderImage({ 
        
        img_dim <- str_split(input$size, "x", n = 1)
        
        url <- create_image(
                prompt = input$depict,
                n = 1,
                size = input$size,
                openai_api_key = input$key2
              )[["data"]][1, ]

        filename <- paste("image-", str_replace_all(str_sub(Sys.time(), 1, 19), ":", ""), ".png", sep = "")
        download.file(url, filename, mode = "wb")
        
        list(src = filename, contentType = "image/png", alt = filename, 
             width = img_dim, height = img_dim
        )
        
      }, deleteFile = FALSE)
        
    },
    error = function(error_message){
      showModal(modalDialog(
        title = "Error!",
        as.character(error_message),
        footer = modalButton("Ok"),
        fade = TRUE
      ))
        
    })
    
  })
  
  
  observeEvent(input$navset, {
    
    if(input$navset == "Gallery") {
      
      output$images <- renderUI({ 
        
        tryCatch({
          
          image_files <- list.files(path = ".", pattern = ".png", all.files = FALSE, full.names = TRUE)
          lapply(image_files, 
                 function(path) {
                   
                   renderImage({
                     list(src = path, alt = path)
                   }, deleteFile = FALSE)
                   
                 }
          )
        },
        error = function(error_message){
          showModal(modalDialog(
            title = "Error!",
            as.character(error_message),
            footer = modalButton("Ok"),
            fade = TRUE
          ))
          
        })
        
      })
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)