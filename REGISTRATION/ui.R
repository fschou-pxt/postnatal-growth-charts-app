library(shiny)
library(shinyjs)
library(metathis)

shinyUI(fluidPage(style="max-width:800px;",
    useShinyjs(),
    meta() %>% meta_viewport(maximum_scale = 1),
    tags$style("@import url('https://fonts.googleapis.com/css2?family=Nunito:wght@400&display=swap');
                       #weight_header:hover {background-color:white;}"),
    tags$style(".section-title {font-weight:900;color:#546e7a;text-decoration:underline;}"),
    tags$style(".title {font-weight:900;color:#546e7a;padding:10px;border:2px solid #546e7a;border-radius:12px;line-height:150%;}"),
    
    
    fluidRow(
        column(12,
        style="font-family:'Nunito';",
        h3(HTML("NICU Postnatal Growth Charts for Preterm Infants<br/>ACCOUNT REGISTRATION"), class="title"),
        hr(),
        div(style="padding:20px;border:2px solid #546e7a;border-radius:12px;",
        h3("USER INFO", class="section-title"),
        div(align="left", style="padding:10px;font-size:16px;",
            radioButtons("registrantType", NULL, choices = c("Parent", "Neonatal Care Provider"), selected = "", inline = TRUE),
            uiOutput("registrantType_msg")),
            br(),
            fluidRow(
                div(class = "col-lg-6 col-md-6 col-sm-6",
                    textInput("firstName", span("First Name", span(style="color:red;font-weight:400;", " (required)")), width = "100%"),
                    uiOutput("firstName_msg")),
                div(class = "col-lg-6 col-md-6 col-sm-6",
                    textInput("lastName", span("Last Name", span(style="color:red;font-weight:400;", " (required)")), width = "100%"),
                    uiOutput("lastName_msg"))
            ),
            br(),
            div(textInput("gmail", 
                      span("E-Mail Address", span(style="color:red;font-weight:400;", " (required)"), span(em("For use in future communications regarding your account."), style="color:red;font-weight:400;font-size:12px;")), width = "100%"),
            uiOutput("gmail_msg")),
            br(),
            fluidRow(
                div(class = "col-lg-6 col-md-6 col-sm-6",
                    textInput("affiliation", "Affiliation", width = "100%"),
                    uiOutput("affiliation_msg")),
                div(class = "col-lg-6 col-md-6 col-sm-6",
                    textInput("title", "Job Title", width = "100%"),
                    uiOutput("title_msg"))
            ),
            br(),
            br(),
            h3("ACCOUNT SETUP", class="section-title"),
            fluidRow(
                div(class = "col-lg-4 col-md-4 col-sm-4",
                    textInput("username", span("Pick a User ID", span(style="color:red;font-weight:400;", " (required)")), width = "100%"),
                    uiOutput("username_msg")),
                div(class = "col-lg-4 col-md-4 col-sm-4",
                    passwordInput("pwd", span("Enter Password", span(style="color:red;font-weight:400;", " (required)")), width = "100%"),
                    uiOutput("pwd_msg")),
                div(class = "col-lg-4 col-md-4 col-sm-4",
                    passwordInput("pwd_repeat", span("Re-enter Password", span(style="color:red;font-weight:400;", " (required)")), width = "100%"),
                    uiOutput("pwd_repeat_msg"))
            ),
            br(),
            hr(),
            div(align="right", actionButton("submit", "SUBMIT", style="font-size:16px;background-color:#546E7A;color:#FFFFFF;border:0;")),
        br(),
        uiOutput("message"))
        )
    )
    
))
