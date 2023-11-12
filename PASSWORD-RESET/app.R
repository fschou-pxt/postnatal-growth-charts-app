library(safer)
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  div(style="margin:20px;border:2px inset #aaaaaa;border-radius:3px;padding:0 20px 10px 20px;width:400px;font-family:tahoma sans-serif;",
      h3("nicugrowth.app password reset"),
      hr(),
      textInput("code", "Code received in E-Mail", width = "100%"),
  passwordInput("new_password", "New Password", width = "100%"),
  passwordInput("retype_new_password", "Retype New Password", width = "100%"),
  hr(),
  actionButton("submit", "SUBMIT"),
  div(style="padding-top:10px;", uiOutput("message"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$submit, {
    id <- read_csv("../REGISTRATION/www/user.csv")$id
    
    if (input$new_password == input$retype_new_password) {
      output$message <- renderUI({})
      if (input$code == "") {
        output$message <- renderUI({
          span(style="color:red;", "Enter a code!")
        })
      } else if (class(try(decrypt_string(input$code, "nicugrowth.app"))) %in% "try-error") {
        output$message <- renderUI({
          span(style="color:red;", "Code in valid!")
        })
      } else if (decrypt_string(input$code, "nicugrowth.app") %in% id) {
        write_csv(data.frame(pwd = encrypt_string(input$new_password, decrypt_string(input$code, "nicugrowth.app"))),
                  "../REGISTRATION/www/pwd.csv", append = TRUE)
        output$message <- renderUI({
          div(style="color:red;", span("Password reset!", br(), "Close the window and return to the", 
                                       a("Sign-In", href = "https://sites.google.com/nicugrowth.app/nicugrowth-app-full/webapp?authuser=0", style="color:red;font-weight:600;"), "page."))
        })
      } 
    }  else {
      output$message <- renderUI({
        span(style="color:red;", "Entered passwords do not match!")
      })
    }
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
