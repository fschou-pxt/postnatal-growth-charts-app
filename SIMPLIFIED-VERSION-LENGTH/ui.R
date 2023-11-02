library(shiny)
library(shinydisconnect)
library(shinyjs)
library(metathis)

# Define UI for application that draws a histogram
shinyUI(
  fixedPage(style="margin-top:25px;padding-top:25px;border-left:1px solid #eeeeee;border-top:1px solid #eeeeee;border-right:1px solid #cccccc;border-bottom:1px solid #888888;",
            tags$style(".col-sm-3 {padding:5px !important;margin:0 !important;}"),
            useShinyjs(),
            meta() %>% meta_viewport(maximum_scale = 1),
        
        disconnectMessage(
            text = "Current session has timed out. Please refresh the page.",
            refresh = "",
            background = "#646464E6",
            colour = "#FFFFFF",
            refreshColour = "#337AB7",
            overlayColour = "#999999",
            overlayOpacity = 0.6,
            width = "full",
            top = "center",
            size = 18,
            css = ""
        ),
        
        fluidRow(style="font-size:16px;margin:0;",
                 div(class="col-lg-12 col-md-12 col-sm-12", strong(tags$u("Length Trajectory", style="font-size:24px;")), hr()),
                 div(class="col-lg-3 col-md-3 col-sm-3", selectInput("GA_week", "GA (week)", choices = c("", c(22:34)), width = "100%")),
                 div(class="col-lg-3 col-md-3 col-sm-3", selectInput("GA_day", "GA (day)", choices = c("", c(0:6)), width = "100%")),
                 div(class="col-lg-3 col-md-3 col-sm-3", selectInput("sex", "Sex", choices = c("", "Female", "Male"), width = "100%")),
                 div(class="col-lg-3 col-md-3 col-sm-3", actionButton("plot", "READY-SET-GO", width = "100%", style="margin-top:27px;margin-bottom:27px;font-size:16px;background-color:#546E7A;color:#FFFFFF;border:0;"))
                         ),
        fluidRow(style="font-size:12px;margin:0;",
                 div(class="col-lg-5 col-md-5 col-sm-5",
                     uiOutput("table"),
                     uiOutput("msg"),
                     br()
                 ),
                 div(class="col-lg-7 col-md-7 col-sm-7",
                     plotOutput("length_plot", height = "410px")
                 )
        ),
        br()
        
        
    )
    
)
