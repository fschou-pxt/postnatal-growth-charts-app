library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinydisconnect)
library(shinyWidgets)
library(plotly)
library(DT)
library(metathis)

shinyUI(
  fluidPage(style="max-width:1200px;",
            dashboardPage(
              dashboardHeader(disable = TRUE),
              dashboardSidebar(disable = TRUE),
              dashboardBody(style="font-family:'Nunito';",
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
                            
                            tags$head(
                              tags$link(rel='stylesheet', type = 'text/css', href='style.css')
                            ),
                            tags$script('src="https://kit.fontawesome.com/a175aacf3d.js"'),
                            setSliderColor(rep("#546E7A",3), c(1,2,3)),
                            
                            fluidRow(id = "main_frame",
                                     div(class="col-lg-12 col-md-12 col-sm-12",
                                         div(style="margin-left:16px;padding:0;", class="greetings", 
                                             span(style="font-size:14px;font-weight:400;", "Postnatal Growth Charts for Preterm Infants")
                                         )),
                                     div(class="col-lg-6 col-md-6 col-sm-12", style="padding:0;",
                                         div(style="margin:16px;", class="greetings", 
                                             #span(style="font-size:16px;font-weight:400;", "NICU Postnatal Growth Charts for Preterm Infants"),
                                             uiOutput("greeting"))),
                                     div(class="col-lg-6 col-md-6 col-sm-12", style="padding:0;",
                                         div(align="right", style="margin:15px;",
                                             hidden(actionButton("settings_button", "", icon = tags$i(class="fa-solid fa-gear"), class = "gtc-button")),
                                             hidden(actionButton("restore_box", "RESTORE ALL PANELS", class = "gtc-button", icon = tags$i(class="fa-solid fa-diagram-predecessor")))
                                         ))),
                            uiOutput("settings"),
                            box(id = "instructions",
                                title = span(span("INSTRUCTIONS"), class = "box-header-title"),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = FALSE,
                                solidHeader = TRUE,
                                width = 12,
                                fluidRow(
                                  column(12, 
                                         div(style="padding:16px;",
                                             span("WebApp Guides", class = "box-header-title"),
                                             HTML('
                                        <div style="line-height:200%;">
                                          <ul style="padding-left:8px;">
                                            <i class="fa-solid fa-pencil"></i> Please sign in using your registered account. If you do not have an account yet, please click <a href="https://shiny.nicugrowth.app/webapp-registration/" target="_blank">here</a> to register.<br/>
                                            <i class="fa-solid fa-pencil"></i> After signing in, select an infant from the list, or add an infant by selecting <span class="instructionNonButton">CREATE A NEW ID</span>.<br/>
                                            <i class="fa-solid fa-pencil"></i> To load the anthropometric data table, click <span class="instructionButton">READY&#8209;SET&#8209;GO</span>.<br/>
                                            <i class="fa-solid fa-pencil"></i> To add/modify/delete a measurement value, double-click on the corresponding table cell.<br/>
                                            <i class="fa-solid fa-pencil"></i> To save the data table, click <span class="instructionButton">SAVE</span>.<br/>
                                            <i class="fa-solid fa-pencil"></i> To delete the entire record for the selected infant, click <span class="instructionButton">DELETE</span>.<br/>
                                            <i class="fa-solid fa-pencil"></i> To calculate the trajectory percentiles, click <span class="instructionButton">UPDATE</span>.<br/>
                                            <i class="fa-solid fa-pencil"></i> To assess weight growth and obtain calories increase recommendation, click <span class="instructionButton">ASSESS&nbsp;WEIGHT</span>.<br/>
                                            <i class="fa-solid fa-pencil"></i> To download the data table or the plots, expand <span class="instructionButton">OUTPUT</span> box by clicking on the plus sign on the right side.<br/>
                                            <i class="fa-solid fa-pencil"></i> To download the plots, select <span class="instructionNonButton">Paper Size</span>, <span class="instructionNonButton">Orientation</span>, and <span class="instructionNonButton">Layout</span>, then click <span class="instructionButton">DOWNLOAD&nbsp;PLOT</span>. <br/>
                                            <i class="fa-solid fa-pencil"></i> To download the data table, select <span class="instructionNonButton">File Format</span> and <span class="instructionNonButton">Columns</span> to download, then click <span class="instructionButton">DOWNLOAD&nbsp;TABLE</span>. 
                                          </ul>
                                        </div>'
                                             ))),
                                  column(12,
                                         div(style="padding:16px;",
                                             span("Growth Curve Guides", class = "box-header-title"),
                                             div(style="line-height:200%;",
                                                 tags$ul(style="padding-left:8px;",
                                                         column(12,
                                                                span(HTML(paste0(tags$i(class="fa-solid fa-circle-exclamation", style="color:red;"), 
                                                                                 " Percentile lines from the bottom to the top represent: 3", tags$sup("rd"),
                                                                                 ", 10", tags$sup("th"),
                                                                                 ", 25", tags$sup("th"),
                                                                                 ", 50", tags$sup("th"),
                                                                                 ", 75", tags$sup("th"),
                                                                                 ", 90", tags$sup("th"),
                                                                                 ", 97", tags$sup("th"),
                                                                                 " percentiles.")))),
                                                         column(12,
                                                                span(tags$i(class="fa-solid fa-circle-exclamation", style="color:red;"),
                                                                     " Trajectory percentile grid search is performed between 0.1 and 99.9 percentile. ≤0.1 and ≥99.9 is used to indicate uncertainty beyond the tested upper and lower limits.")),
                                                         column(12,
                                                                span(tags$i(class="fa-solid fa-circle-info", style="color:#ffbf00;"),
                                                                     " Weight acceleration phase growth rate (g/kg/day): rate of weight gain for corresponding trajectory percentile between BW regained and 34 weeks PMA.")),
                                                         column(12,
                                                                span(tags$i(class="fa-solid fa-circle-info", style="color:#ffbf00;"),
                                                                     " Stable weight gain phase growth rate (g/day): rate of weight gain for corresponding trajectory percentile after 34 weeks PMA."))))
                                         )))),
                            box(id = "input_box",
                                title = span("INPUT", class = "box-header-title"),
                                collapsible = TRUE,
                                closable = FALSE,
                                width = 12,
                                fluidRow(
                                  div(class="col-lg-3 col-md-4 col-sm-5", style="height:480px;",
                                      tabsetPanel(id="main", type = "hidden",
                                                  tabPanel("login",
                                                           div(style="font-size:11px;color:red;word-break:normal;", "Forgot password?", a(href="mailto:admin@nicugrowth.app", tags$u("Send us"), style="text-decoration:none;color:red;"), " a message and we will send you a link to reset your password."),
                                                           br(),
                                                           textInput("id", "USER ID", value = ""),
                                                           passwordInput("password", "PASSWORD", value = ""),
                                                           hr(),
                                                           div(align="left",
                                                               checkboxInput("edit_mode", span("EDITING MODE", style="font-size:12px;", class = "meas-box")),
                                                               checkboxInput("admitted", span("SHOW ADMITTED ONLY", style="font-size:12px;", class = "meas-box"), value = TRUE),
                                                           ),
                                                           div(align="right",
                                                               actionButton("signin", icon = tags$i(class="fa-solid fa-right-to-bracket"), "SIGN IN", class = "gtc-button"),br(),
                                                               a(href="https://shiny.nicugrowth.app/webapp-registration/", target="_blank", actionButton("signup", icon = tags$i(class="fa-solid fa-user-plus"), "SIGN UP", class = "gtc-button"))
                                                           ),
                                                           hr(),
                                                           div(style="font-size:11px;", "Not ready to sign up for an account? Follow the links below to use the simplified version of the postnatal growth charts:", br(),
                                                               div(a(href='https://shiny.nicugrowth.app/webapp-weight/', target='_blank', "Weight"), "|", a(href='https://shiny.nicugrowth.app/webapp-length/', target='_blank', "Length"),  "|", a(href='https://shiny.nicugrowth.app/webapp-headcirc/', target='_blank', "Head Circumference"))),
                                                           br(),
                                                           uiOutput("signin_msg")                                              
                                                  ),
                                                  tabPanel("selector",
                                                           div(id = "id_select_div", style="padding:5px 5px 0 5px;border-radius:5px;background-color:#546E7A;color:#FFFFFF;", 
                                                               uiOutput("id_selector")
                                                           ),
                                                           div(id = "GA_sex_select_div", style="padding:5px 5px 0 5px;border-radius:5px;background-color:#7895a2;color:#FFFFFF;", 
                                                               selectInput("GA_week", "GA (week)", choices = c("", c(22:34)), width = "100%", selected = ""),
                                                               selectInput("GA_day", "GA (day)", choices = c("", c(0:6)), width = "100%", selected = ""),
                                                               selectInput("sex", "Sex", choices = c("", "Female", "Male"), width = "100%")
                                                           ),
                                                           hr(),
                                                           div(hidden(checkboxInput("discharged", "Check if discharged"))),
                                                           div(actionButton("plot", icon = tags$i(class="fa-solid fa-jet-fighter-up"), span("READY-SET-GO", tags$i(class="fa-solid fa-jet-fighter-up")), width = "100%", class = "gtc-button large-padding")),
                                                           br(),
                                                           uiOutput("msg")
                                                  ))
                                  ),
                                  div(class="col-lg-9 col-md-8 col-sm-7 demo-wrap", style="height:480px;",
                                      fluidRow(class="demo-content",
                                               div(id = "meas_table_view", DTOutput("meas_table"), style = "height:440px;padding-right:12px;padding-left:12px;font-size:11px;"),
                                               div(align="right", style="padding:8px 15px 0 0;",
                                                   hidden(actionButton("delete_record", icon = tags$i(class="fa-solid fa-trash-can"), "DELETE", class = "gtc-button")),
                                                   hidden(actionButton("save_data", icon = tags$i(class="fa-solid fa-floppy-disk"), "SAVE", class = "gtc-button")),
                                                   hidden(actionButton("calculate", icon = tags$i(class="fa-solid fa-chart-line"), "", class = "gtc-button")),
                                                   hidden(actionButton("weight_assess", icon = tags$i(class="fa-solid fa-weight-scale"), "ASSESS WEIGHT", class = "gtc-button-2")
                                                          
                                                   ))))
                                )
                            ),
                            box(id = "weight_box",
                                title = span("WEIGHT", class = "box-header-title"),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = FALSE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         div(class="col-lg-8 col-md-8 col-sm-6",
                                             plotlyOutput("weight_plot", height = "480px"),
                                             br(),
                                         ),
                                         div(class="col-lg-4 col-md-4 col-sm-6",
                                             div(class="meas-info-box",
                                                 div(style="padding:20px;font-size:16px;",
                                                     div(sliderInput("weight_range", HTML("Set DOL Range"), min = 0, max = 308, value = c(0, 308))),
                                                     div(actionButton("BW_regained", "BW regained", class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;"),
                                                         actionButton("first_twentyeight", "0-27d", class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;"),
                                                         actionButton("last_seven", "Last 7d", class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;"),
                                                         actionButton("seven_prior", HTML("< last 7d"), class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;"),
                                                         actionButton("all", "All", class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;")),
                                                     div(HTML("<hr class='line'>")),
                                                     div("Trajectory Percentile (z-score):", style="font-weight:600;"),
                                                     uiOutput("weight_trajectory_percentile"),
                                                     br(),
                                                     div("Transition to WHO Charts at: ", style="font-weight:600;"),
                                                     uiOutput("weight_WHO_transition"),
                                                     br(),
                                                     div("Weight acceleration phase:", style="font-weight:600;font-size:14px;"),
                                                     uiOutput("rate_acceleration"),
                                                     div("Stable weight gain phase:", style="font-weight:600;font-size:14px;"),
                                                     uiOutput("rate_stable")
                                                 )
                                             )
                                         )
                                )
                                
                                
                            ),
                            
                            box(id = "length_box",
                                title = span("LENGTH", class = "box-header-title"),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = FALSE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         div(class="col-lg-8 col-md-8 col-sm-6",
                                             plotlyOutput("length_plot", height = "480px"),
                                             br()
                                         ),
                                         div(class="col-lg-4 col-md-4 col-sm-6",
                                             div(class="meas-info-box",
                                                 div(style="padding:20px;font-size:16px;",
                                                     div(sliderInput("length_range", HTML("Set DOL Range"), min = 0, 308, value = c(0,308))),
                                                     div(actionButton("all_length", "All", class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;")),
                                                     div(HTML("<hr class='line'>")),
                                                     div("Trajectory Percentile (z-score):", style="font-weight:600;"),
                                                     uiOutput("length_trajectory_percentile"),
                                                     br(),
                                                     div("Transition to WHO Charts at: ", style="font-weight:600;"),
                                                     uiOutput("length_WHO_transition"))
                                             )
                                         )
                                )
                                
                            ),
                            
                            box(id = "HC_box",
                                title = span("HEAD CIRCUMFERENCE", class = "box-header-title"),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = FALSE,
                                width = 12,
                                fluidRow(class="meas-box",
                                         div(class="col-lg-8 col-md-8 col-sm-6",
                                             plotlyOutput("HC_plot", height = "480px"),
                                             br()
                                         ),
                                         div(class="col-lg-4 col-md-4 col-sm-6",
                                             div(class="meas-info-box",
                                                 div(style="padding:20px;font-size:16px;",
                                                     div(sliderInput("HC_range", HTML("Set DOL Range"), min = 0, 308, value = c(0,308))),
                                                     div(actionButton("all_HC", "All", class = "gtc-button", style="font-size:12px;padding:3px;font-weight:700;")),
                                                     div(HTML("<hr class='line'>")),
                                                     div("Trajectory Percentile (z-score):", style="font-weight:600;"),
                                                     uiOutput("HC_trajectory_percentile"),
                                                     br(),
                                                     div("Transition to WHO Charts at: ", style="font-weight:600;"),
                                                     uiOutput("HC_WHO_transition"))
                                             )
                                         )
                                )
                                
                            ),
                            
                            box(id = "output_box",
                                title = span("OUTPUT", class = "box-header-title"),
                                collapsible = TRUE,
                                collapsed = TRUE,
                                closable = FALSE,
                                width = 12,
                                fluidRow(class="meas-box", style="padding:12px;",
                                         accordion(id="output_accordion",
                                                   accordionItem(title = span("Plot", class = "box-header-title"),
                                                                 collapsed = FALSE,
                                                                 fluidRow(
                                                                   div(class="col-lg-3 col-md-3 col-sm-3",
                                                                       radioButtons("plot_output_size", "Paper Size",
                                                                                    choices = c("Letter", "Legal", "A4", "B5"),
                                                                                    selected = character(0)),
                                                                       radioButtons("plot_output_orientation", "Orientation",
                                                                                    choices = c("Landscape", "Portrait"),
                                                                                    selected = character(0))),
                                                                   div(class="col-lg-9 col-md-9 col-sm-9",
                                                                       div("Layout", style="font-size:14px;font-weight:700;margin-bottom:5px;"),
                                                                       actionGroupButtons(c("plot_layout_1", "plot_layout_2", "plot_layout_3"),
                                                                                          labels = list(
                                                                                            tags$img(src="plate-layout.png", style="height:50px;"),
                                                                                            tags$img(src="vertical-layout.png", style="height:50px;"),
                                                                                            tags$img(src="horizontal-layout.png", style="height:50px;")
                                                                                          )),
                                                                       hr(),
                                                                       div(align="left", 
                                                                           fluidRow(
                                                                             column(8,uiOutput("download_plot_msg")),
                                                                             column(align="right", 4, downloadButton("download_plot", "DOWNLOAD PLOT"))
                                                                           )
                                                                       ))
                                                                 )
                                                   ),
                                                   accordionItem(title = span("Table", class = "box-header-title"),
                                                                 collapsed = FALSE,
                                                                 fluidRow(
                                                                   div(class="col-lg-3 col-md-3 col-sm-3",
                                                                       radioButtons("table_output_format", "File Format",
                                                                                    choices = c("CSV", "MS Excel", "MS Word"),
                                                                                    selected = character(0))),
                                                                   div(class="col-lg-9 col-md-9 col-sm-9",
                                                                       selectInput("table_output_columns", "Select columns to download (arranged by selection order)",
                                                                                   multiple = TRUE,
                                                                                   choices = c("DOL", "PMA", "Week", "Day", "Weight", "Weight Z (%ile)", "Length", "Length Z (%ile)", "HC", "HC Z (%ile)"),
                                                                                   selected = c("DOL", "PMA", "Week", "Day", "Weight", "Weight Z (%ile)", "Length", "Length Z (%ile)", "HC", "HC Z (%ile)")),
                                                                       div(align="left", 
                                                                           fluidRow(
                                                                             column(8,uiOutput("download_table_msg")),
                                                                             column(align="right", 4, downloadButton("download_table", "DOWNLOAD TABLE"))
                                                                           )),
                                                                       br())
                                                                 ))))
                                
                            ),
                            fluidRow(
                              column(12,
                                     style="height:50px;")
                            ),
                            
                            actionButton("backtotop", "BACK TO TOP", 
                                         style="z-index:10000;position:fixed;bottom:0;right:0;margin-bottom:10px;
                     margin-right:9px;padding:3px 6px 3px 6px;
                     border:1px solid black;border-radius:50px;font-size:14px;background-color:white;
                     box-shadow:2px 2px 3px #bbbbbb;")
                            
              )
              
            ))
)
