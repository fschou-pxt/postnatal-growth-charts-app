library(shiny)
library(tidyverse)
library(safer)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    rv <- reactiveValues(registrantType = 0, firstName = 0, lastName = 0, gmail = 0, username = 0, pwd = 0)
    
    observeEvent(input$submit, {
        if (is.null(input$registrantType)) {
            output$registrantType_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Must select one.")
            })
            rv$registrantType <- 1
        } else {
            output$registrantType_msg <- renderUI({
            })
            rv$registrantType <- 0
        }
        
        if (input$firstName == "") {
            output$firstName_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Must enter First Name.")
            })
            rv$firstName <- 1
        } else {
            output$firstName_msg <- renderUI({
            })
            rv$firstName <- 0
        }
        
        if (input$lastName == "") {
            output$lastName_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Must enter Last Name.")
            })
            rv$lastName <- 1
        } else {
            output$lastName_msg <- renderUI({
            })
            rv$lastName <- 0
        }
        
        if (input$gmail == "") {
            output$gmail_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Must provide an E-Mail address.")
            })
            rv$gmail <- 1
        } else {
            output$gmail_msg <- renderUI({
            })
            rv$gmail <- 0
        }
        
        
        id <- read_csv("www/user.csv")
        pwd <- read_csv("www/pwd.csv")
        
        if (input$username == "") {
            output$username_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Must enter a username!")
            })
            rv$username <- 1
        } else if (input$username %in% id$id) {
            output$username_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Username already exists!")
            })
            rv$username <- 1
        } else {
            output$username_msg<- renderUI({})
            rv$username <- 0
            
        }
        
        if (input$pwd == "") {
            output$pwd_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Must enter a password!")
            })
            rv$pwd <- 1        
        } else if (!(input$pwd %in% input$pwd_repeat)) {
            output$pwd_msg <- renderUI({})
            output$pwd_repeat_msg <- renderUI({
                div(style="color:red;padding-bottom:20px;", "Password does not match!")
            })
            rv$pwd <- 1
        } else if (input$pwd %in% input$pwd_repeat) {
            output$pwd_msg <- renderUI({})
            output$pwd_repeat_msg <- renderUI({})
            rv$pwd <- 0
        }
        
        
        if (rv$registrantType + rv$firstName + rv$lastName + rv$gmail + rv$username + rv$pwd == 0) {
            temp <- data.frame(
                timestamp = Sys.time(),
                timezone = Sys.timezone(),
                registrantType = input$registrantType,
                firstName = input$firstName,
                lastName = input$lastName,
                gmail = input$gmail,
                affiliation = input$affiliation,
                title = input$title,
                username = input$username,
                approved = NA,
                access_granted = NA
            )
            disable("submit")
            
            write_csv(temp, "www/registration.csv", append = TRUE)
            write_csv(data.frame(id = input$username, 
                                 color_theme = 1, 
                                 font = "Nunito", 
                                 background = "[Blank]"), "www/user.csv", append = TRUE)
            
            write_csv(data.frame(pwd = encrypt_string(input$pwd, key = input$username)), "www/pwd.csv", append = TRUE)
            
            showModal(
              modalDialog(
                title = "Registration Confirmation",
                div(style="color:#54617a;font-size:14px;font-weight:500;padding:36px 12px 12px 12px;",  align="center",
                    div("Thank you for registering! Your account has been created successfully!", style="font-size:20px;font-weight:600;padding-bottom:6px;") 
                ),
                div(style="font-size:11px;padding-top:12px;", align="center", em("Please close this window and return to the sign-in page."))
                )
            )

        }
        
        
        
    })
    
})
