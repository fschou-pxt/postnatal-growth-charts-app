library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyWidgets)
library(DT)

graph_choices <- c("Choose One..." = "", 
                   "Plot raw data by DOL for each GA group" = "1",
                   "Plot Z-score-transformed data by DOL" = "2",
                   "Plot Percentile-transformed data by DOL" = "3",
                   "Trajectory Percentile Comparison" = "4", 
                   "Delta Z between first and last measurement" = "5",
                   "Delta Z between first and measurement at PMA 36 weeks or last measurement if before PMA 36 weeks" = "6")

shinyUI(
  fluidPage(style="max-width:1200px;",
            dashboardPage(
              dashboardHeader(disable = TRUE),
              dashboardSidebar(disable = TRUE),
              dashboardBody(style="font-family:'Nunito';",
                            useShinyjs(),
                            tags$head(
                              tags$link(rel='stylesheet', type = 'text/css', href='style.css')
                            ),
                            tags$script('src="https://kit.fontawesome.com/a175aacf3d.js"'),
                            setSliderColor(rep("#546E7A",3), c(1,2,3)),
                            fluidRow(
                              div(class="col-lg-6 col-md-6 col-sm-6",
                                  div(style="margin:15px;", class="greetings", uiOutput("greeting"))),
                              div(class="col-lg-6 col-md-6 col-sm-6",
                                  div(align="right", style="margin:15px;",
                                      actionButton("restore_box", "RESTORE ALL PANELS", class = "gtc-button", icon = tags$i(class="fa-solid fa-diagram-predecessor")),
                                      actionButton("settings_button", "OPEN SETTINGS", icon = tags$i(class="fa-solid fa-gear"), class = "gtc-button")))),
                            uiOutput("settings"),
                            box(id = "input_box",
                                title = span("INPUT", class = "box-header-title"),
                                collapsible = TRUE,
                                closable = TRUE,
                                width = 12,
                                fluidRow(
                                  
                                  div(class="col-lg-3 col-md-3 col-sm-3", #style="height:680px;",
                                      tabsetPanel(id="main", type = "hidden",
                                                  tabPanel("login",
                                                           div(style="font-size:11px;color:red;word-break:normal;", "Forgot password? Send us a message, and we will send you a link to reset the password."),
                                                           br(),
                                                           textInput("id", "USER ID"),
                                                           passwordInput("password", "PASSWORD"),
                                                           hr(),
                                                           div(align="right",
                                                               actionButton("signin", icon = tags$i(class="fa-solid fa-right-to-bracket"), "SIGN IN", class = "gtc-button")
                                                           ),
                                                           br(),
                                                           uiOutput("signin_msg")
                                                  ),
                                                  tabPanel("uploader",
                                                           div(id = "id_select_div", style="padding:5px 5px 0 5px;border-radius:5px;background-color:#546E7A;color:#FFFFFF;", 
                                                               fileInput("upload_file", "Upload File", 
                                                                         buttonLabel = tags$i(class = "fa-solid fa-upload"), 
                                                                         placeholder = "Select A File...", 
                                                                         width = "100%",
                                                                         accept = c(".csv", ".xlsx")),
                                                               uiOutput("table_assignment"),
                                                               br()
                                                           ),
                                                           hr(),
                                                           div(hidden(actionButton("plot", icon = tags$i(class="fa-solid fa-jet-fighter-up"), span("TIDY THE DATASET", tags$i(class="fa-solid fa-jet-fighter-up")), width = "100%", class = "gtc-button")),
                                                               br(), br(),
                                                               hidden(actionButton("analyzedTable", icon = tags$i(class="fa-solid fa-file-circle-check"), span("THIS IS A",br(), "POST-ANALYSIS TABLE"), width="100%", class = "gtc-button"))
                                                           ),
                                                           br(),
                                                           uiOutput("msg")))
                                  ),
                                  div(class="col-lg-9 col-md-9 col-sm-9", #style="height:680px;",
                                      fluidRow(
                                        div(uiOutput("meas_table_UI")),
                                        div(column(12, align="right", style="padding:8px 15px 0 0;",
                                                   hidden(checkboxInput("includeTP", "Include trajectory percentile calculation")),
                                                   hidden(actionButton("calculate", icon = tags$i(class="fa-solid fa-chart-line"), "ANALYZE", class = "gtc-button")),
                                                   hidden(downloadButton("save_dt", "DOWNLOAD TABLE", class = "gtc-button"))
                                                   
                                        ))))
                                )
                            ),
                            
                            box(id = "weight_box",
                                title = span("WEIGHT", class = "box-header-title"),
                                collapsible = F,
                                collapsed = F,
                                closable = TRUE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         div(class="col-lg-12 col-md-12 col-sm-12",
                                             selectInput("weight_type_analysis", "Select An Analysis Type",
                                                         width = "100%",
                                                         choices = graph_choices),
                                             uiOutput("weight_UI")
                                         )
                                )
                                
                                
                            ),
                            
                            box(id = "length_box",
                                title = span("LENGTH", class = "box-header-title"),
                                collapsible = F,
                                collapsed = F,
                                closable = TRUE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         div(class="col-lg-12 col-md-12 col-sm-12",
                                             selectInput("length_type_analysis", "Select An Analysis Type",
                                                         width = "100%",
                                                         choices = graph_choices),
                                             uiOutput("length_UI")
                                         )
                                )
                                
                            ),
                            
                            box(id = "HC_box",
                                title = span("HEAD CIRCUMFERENCE", class = "box-header-title"),
                                collapsible = F,
                                collapsed = F,
                                closable = TRUE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         div(class="col-lg-12 col-md-12 col-sm-12",
                                             selectInput("HC_type_analysis", "Select An Analysis Type",
                                                         width = "100%",
                                                         choices = graph_choices),
                                             uiOutput("HC_UI")
                                         )
                                )
                                
                            ),
                            
                            box(id = "output_box",
                                title = span("OUTPUT", class = "box-header-title"),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = TRUE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         accordion(id="output_accordion",
                                                   accordionItem(title = span("Plot", class = "box-header-title"),
                                                                 collapsed = FALSE,
                                                                 fluidRow(
                                                                   div(class="col-lg-4 col-md-4 col-sm-4",
                                                                       radioButtons("plot_output_size", "Paper Size",
                                                                                    choices = c("Letter", "Legal", "A4", "B5"),
                                                                                    selected = character(0)),
                                                                       radioButtons("plot_output_orientation", "Orientation",
                                                                                    choices = c("Landscape", "Portrait"),
                                                                                    selected = character(0))),
                                                                   div(class="col-lg-8 col-md-8 col-sm-8",
                                                                       div("Layout", style="font-size:14px;font-weight:700;margin-bottom:5px;"),
                                                                       actionGroupButtons(c("plot_layout_1", "plot_layout_2", "plot_layout_3"), 
                                                                                          labels = list(
                                                                                            tags$img(src="plate-layout.png", style="height:75px;"),
                                                                                            tags$img(src="vertical-layout.png", style="height:75px;"),
                                                                                            tags$img(src="horizontal-layout.png", style="height:75px;")
                                                                                          )),
                                                                       hr(),
                                                                       div(align="left", 
                                                                           uiOutput("download_plot_msg"),
                                                                           downloadButton("download_plot", "DOWNLOAD PLOT", class = "gtc-button")))
                                                                 )
                                                   ),
                                                   accordionItem(title = span("Table", class = "box-header-title"),
                                                                 collapsed = FALSE,
                                                                 fluidRow(
                                                                   div(class="col-lg-4 col-md-4 col-sm-4",
                                                                       radioButtons("table_output_format", "File Format",
                                                                                    choices = c("CSV", "MS Excel", "MS Word"),
                                                                                    selected = character(0))),
                                                                   div(class="col-lg-8 col-md-8 col-sm-8",
                                                                       selectInput("table_output_columns", "Select columns to download (arranged by selection order)",
                                                                                   multiple = TRUE,
                                                                                   choices = c("DOL", "PMA", "Week", "Day", "Weight", "Weight Z (%ile)", "Length", "Length Z (%ile)", "HC", "HC Z (%ile)"),
                                                                                   selected = c("DOL", "PMA", "Week", "Day", "Weight", "Weight Z (%ile)", "Length", "Length Z (%ile)", "HC", "HC Z (%ile)")),
                                                                       div(align="left", 
                                                                           uiOutput("download_table_msg"),
                                                                           downloadButton("download_table", "DOWNLOAD TABLE", class = "gtc-button")),
                                                                       br())
                                                                 ))))
                                
                            )
                            
              )
              
            ))
)
