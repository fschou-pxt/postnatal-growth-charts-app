library(shiny)
library(safer)
library(tidyverse)

ui <- fixedPage(style="font-family:tahoma;",
                
                tags$style(".step-box {margin:6px;padding:10px;border:1px solid black;border-radius:3px;}"),
                tags$style(".section-title {font-size:24px;font-weight:600px;padding-bottom:10px;"),
                h2("nicugrow.app password reset request procedure", style="font-weight:700;"),
                hr(),
                div(class = "step-box",
                    fluidRow(
                      column(3,
                             span(class = "section-title", "Step 1")),
                      column(8,
                             fluidRow(
                               column(9,textInput("email", "E-Mail Address")),
                               column(3,align="right",
                                      actionButton("step2_go", "GO TO STEP 2", style="margin-top:24px;"))
                             )
                      )
                    )),
                div(class="step-box",
                    fluidRow(
                      column(3,
                             span(class = "section-title", "Step 2")),
                      column(8,
                             uiOutput("step2"))
                    )
                ),
                div(class="step-box",
                    fluidRow(
                      column(3,
                             span(class = "section-title", "Step 3")),
                      column(8,
                             uiOutput("step3"))
                    )
                ),
                uiOutput("step4_all"),
                uiOutput("generate_code"),
                
)

server <- function(input, output) {
  
  rv <- reactiveValues()
  
  observeEvent(input$step2_go, {
    
    registration <- read_csv("../REGISTRATION/www/registration.csv") %>% filter(gmail %in% input$email)
    username <- registration$username
    output$step2 <- renderUI({
      selectInput("select_username", "Select a username associated with this E-Mail account", choices = c("Select One..." = "", username), width = "100%")
    })
  })
  
  observeEvent(input$select_username, {
    pwd <- read_csv("../REGISTRATION/www/pwd.csv")$pwd
    catch <- c()
    for (i in c(1:length(pwd))) {
      if (class(try(decrypt_string(pwd[i], input$select_username))) != "try-error") {
        catch <- c(catch, pwd[i])
      }
    }
    
    if (length(catch) == 1 & input$select_username != "") {
      rv$passwd_df <- data.frame(id = "Password 1", pwd = catch)
      
      output$step3 <- renderUI({
        fluidRow(
          column(9,
                 span("only 1 password associated with this account identified")
          ),
          column(3,align="right",
                 actionButton("delete", "DELETE PASSWORD"))
        )
      })
      output$step4_all <- renderUI({})
    } else if (length(catch) > 1 & input$select_username != "") {
      rv$passwd_df <- data.frame(id = paste0("Password ", c(1:length(catch))), pwd = catch)
      output$step3 <- renderUI({
        fluidRow(
          column(9,
                 span("More than 1 password associated with this account identified")
          ),
          column(3,align="right",
                 actionButton("step4_go", "GO TO STEP 4"))
        )      })
    } else if (length(catch) == 0 & input$select_username != "") {
      output$step3 <- renderUI({
        span("No password associated with this account identified")
      })
    }
  })
  
  observeEvent(input$step4_go, {
    output$step4_all <- renderUI({
      div(class="step-box",
          fluidRow(
            column(3,
                   span(class = "section-title", "Step 4")),
            column(8,
                   uiOutput("step4"))
          )
      )
    })
    
    output$step4 <- renderUI({
      fluidRow(
        column(9,
               selectInput("select_pwd", "Select a password to cross check", choices = c("Select One..." = "", rv$passwd_df$id)),
               uiOutput("step4_message")
        ),
        column(3,
               uiOutput("step4_go"))
      )
    })
  })
  
  observeEvent(input$select_pwd, {
    id <- read_csv("../REGISTRATION/www/user.csv")$id
    id <- setdiff(id, input$select_username)
    catch <- c()
    for (i in c(1:length(id))) {
      if (class(try(decrypt_string(rv$passwd_df$pwd[rv$passwd_df$id == input$select_pwd], id[i]))) != "try-error") {
        catch <- c(catch, pwd[i])
      }
    }
    
    if (length(catch) == 0 & input$select_pwd != "") {
      output$step4_message <- renderUI({
        span("No other account is associated with this password encryption.")
      })
      output$step4_go <- renderUI({
        actionButton("delete1", "DELETE PASSWORD")
      })
    } else if (length(catch) > 0 & input$select_pwd != "") {
      output$step4_message <- renderUI({
        span("At least 1 other account is associated with this password encryption!")
      })
    }
  })
  
  observeEvent(input$delete, {
    pwd <- read_csv("../REGISTRATION/www/pwd.csv")
    pwd_alt <- pwd %>% filter(!(pwd %in% rv$passwd_df$pwd))
    write_csv(pwd_alt, "../REGISTRATION/www/pwd.csv")
    code <- encrypt_string(input$select_username, "nicugrowth.app")
    
    output$generate_code <- renderUI({
      fluidRow(
        
        column(12,
               hr(),
               span("Code for new password: ", strong(code))
        )
      )
    })
  })
  
  observeEvent(input$delete1, {
    pwd <- read_csv("../REGISTRATION/www/pwd.csv")
    pwd_alt <- pwd %>% filter(!(pwd %in% rv$passwd_df$pwd[rv$passwd_df$id %in% input$select_pwd]))
    write_csv(pwd_alt, "../REGISTRATION/www/pwd.csv")
    code <- encrypt_string(input$select_username, "nicugrowth.app")
    
    output$generate_code <- renderUI({
      fluidRow(
        
        column(12,
               hr(),
               span(input$select_pwd, "deleted!", style="color:red;"),
               br(),
               span("Code for new password: ", strong(code))
        )
      )
    })
  })
}

shinyApp(ui = ui, server = server)
