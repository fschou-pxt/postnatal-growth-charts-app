library(shiny)
library(duckdb)
library(DBI)
library(tidyverse)
library(lubridate)
library(DT)
library(shinyjs)
library(safer)
library(patchwork)
library(gt)

shinyServer(function(input, output, session) {
  
  source("function.R")
  source("analysis-functions/plot-by-DOL.R")
  source("analysis-functions/plot-by-Z.R")
  source("analysis-functions/plot-by-Percentile.R")
  source("analysis-functions/TP-comparison.R")
  source("analysis-functions/delta-Z-first-last.R")
  source("analysis-functions/delta-Z-first-36orLast.R")
  
  options(shiny.maxRequestSize = 50*1024^2)
  
  color_palette <- 
    tribble( 
      ~color1,   ~color2,   ~color3,   ~color4,   ~color5,   ~color6,   ~color7,   ~color8,   ~color9,
      "#F1C232", "#445760", "#546E7A", "#7895A2", "#AAAAAA", "#DDDDDD", "#F0F0F0", "#FFFFFF", "#A34100",
      "#59D3C4", "#002774", "#002774", "#003baf", "#397cff", "#74a3ff", "#c2d7ff", "#FFFFFF", "#FF0BA8", 
      "#ff1b6b", "#61a2e2", "#3e196e", "#99a5d3", "#b4a6cb", "#d0a8c3", "#eca9bb", "#FFFFFF", "#6f7bf7",
      "#000814", "#ba2a4e", "#a5345f", "#903f6f", "#7a497f", "#e8b7e2", "#91a5c3", "#FFFFFF", "#f55a9b",
      "#b8475d", "#623a73", "#805174" ,"#9f6976" ,"#bd8078", "#dc9779", "#faae7b", "#FFFFFF", "#133a94",
      "#080808", "#e65763", "#b01041", "#9c3d45", "#773036", "#d4ac94", "#cb997e", "#FFFFFF", "#57276f",
      "#0091ad", "#b7094c", "#a01a58", "#723c70", "#5c4d7d", "#eef0f2", "#f4f5f6", "#FFFFFF", "#455e89",
      "#cf203e", "#f4f269", "#75bd6f", "#c1dd6b", "#a8d26d", "#1e7662", "#ffeda0", "#FFFFFF", "#cf203e", 
    ) %>% t(.) %>% as.data.frame() %>% rownames_to_column(var = "color_order")
  
  color_df <- data.frame(Theme = character(0))
  
  for (i in c(1:(ncol(color_palette)-1))) {
    color_df[i,"Theme"] <- paste0("<span style='font-size:14px;'>Option ",i,"&nbsp;&nbsp;&nbsp", paste0("<span><i class='fa-solid fa-square' style=color:", color_palette[[paste0("V",i)]], ";></i></span>", collapse = ""),"</span>")
  }
  
  rv <- reactiveValues(Chou_weight = data.frame(), 
                       uploaded_dt = data.frame(),
                       dt = data.frame(),
                       dtTidy = data.frame(),
                       dtZ = data.frame(),
                       dtTP = data.frame(),
                       proxy = data.frame(),
                       offset = NA,
                       percentile_min = NA,
                       saved_infant_id = NA,
                       hasDisplay = 0,
                       setting_html = "",
                       plot_width_x = as.numeric(NA),
                       plot_heigth = as.numeric(NA),
                       layout = "plate",
                       output_table_format = as.character(NA),
                       font = ""
  )
  
  plots <- reactiveValues(
    weight_plot = ggplot(),
    length_plot = ggplot(),
    HC_plot     = ggplot(),
    layout      = ggplot()
  )
  
  colors <- reactiveValues(
    color1 = color_palette[1, "V1"],
    color2 = color_palette[2, "V1"],
    color3 = color_palette[3, "V1"],
    color4 = color_palette[4, "V1"],
    color5 = color_palette[5, "V1"],
    color6 = color_palette[6, "V1"],
    color7 = color_palette[7, "V1"],
    color8 = color_palette[8, "V1"],
    color9 = color_palette[9, "V1"],
  )
  
  updateBox("output_box", "remove")
  
  restore <- function() {
    rv$dt <-
      data.frame(
        DOL = numeric(0),
        PMA = numeric(0),
        Weight = numeric(0),
        Weight_Z = numeric(0), 
        Length = numeric(0), 
        Length_Z = numeric(0), 
        HC = numeric(0), 
        HC_Z = numeric(0)
      ) %>%
      mutate(Week = PMA %/% 7, Day = PMA %% 7)   
    rv$proxy <- rv$dt
    output$table <- renderUI({})
    output$weight_plot <- renderPlotly({})
    output$length_plot <- renderPlotly({})
    output$HC_plot     <- renderPlotly({})
    updateBox("weight_box", action = "toggle")
    updateBox("length_box", action = "toggle")
    updateBox("HC_box", action = "toggle")
    updateSelectInput(session, "GA_week", selected = "")
    updateSelectInput(session, "GA_day", selected = "")
    updateSelectInput(session, "sex", selected = "")
    rv$hasDisplay = 0 
    
    
    output$weight_trajectory_percentile <- renderUI({})
    output$weight_WHO_transition <- renderUI({})
    output$length_trajectory_percentile <- renderUI({})
    output$length_WHO_transition <- renderUI({})
    output$HC_trajectory_percentile <- renderUI({})
    output$HC_WHO_transition <- renderUI({})
  }
  
  refresh_saved_infant <- function() {
    con <- dbConnect(duckdb::duckdb(), dbdir="../DATABASE/db.duckdb", read_only=TRUE)
    saved_infant_id <- dbGetQuery(con, paste0("SELECT ID FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'"))
    saved_infant_id <- saved_infant_id$ID
    dbDisconnect(con, shutdown = TRUE)
    return(saved_infant_id)
  }
  
  change_color_palette <- function(selected_color_theme) {
    colors$color1 <- color_palette[1, paste0("V",selected_color_theme)]
    colors$color2 <- color_palette[2, paste0("V",selected_color_theme)]
    colors$color3 <- color_palette[3, paste0("V",selected_color_theme)]
    colors$color4 <- color_palette[4, paste0("V",selected_color_theme)]
    colors$color5 <- color_palette[5, paste0("V",selected_color_theme)]
    colors$color6 <- color_palette[6, paste0("V",selected_color_theme)]
    colors$color7 <- color_palette[7, paste0("V",selected_color_theme)]
    colors$color8 <- color_palette[8, paste0("V",selected_color_theme)]
    colors$color9 <- color_palette[9, paste0("V",selected_color_theme)]
  }
  
  change_color_theme <- function() {
    runjs(paste0("$('.skin-blue .main-sidebar').css('background-color','", colors$color3, "')"))
    runjs(paste0("$('.progress-bar').css('background-color','", colors$color6, "')"))
    runjs(paste0("$('.shiny-progress .progress-text').css('background-color','", colors$color6, "').css('opacity','1')"))
    runjs(paste0("$('.content-wrapper').css('background-color','", colors$color7, "')"))
    runjs(paste0("$('.greetings').css('color','", colors$color9, "')"))
    runjs(paste0("$('#id_select_div').css('background-color','", colors$color3, "')"))
    runjs(paste0("$('#GA_sex_select_div').css('background-color','", colors$color4, "')"))
    runjs(paste0("$('.line').css('background-color','", colors$color3, "')"))
    runjs(paste0("$('.meas-box').css('color','", colors$color3, "')"))
    runjs(paste0("$('.msg-box').css('color','", colors$color7, "')"))
    runjs(paste0("$('.box-header-title').css('color','", colors$color3, "')"))
    runjs(paste0("$('.meas-info-box').css('background-color','", colors$color7, "').css('box-shadow','3px 3px 1px ", colors$color5, "')"))
    runjs(paste0("$('.gtc-button').css('color','", colors$color3, "').css('background-color','", colors$color6, "')"))
    runjs(paste0("$('.enlarge-button').css('color','", colors$color3, "').css('background-color','", colors$color6, "')"))
    runjs(paste0("$('.gtc-button:hover').css('color','", colors$color8, "').css('background-color','", colors$color3, "')"))
    runjs(paste0("$('.gtc-button-rev').css('color','", colors$color8, "').css('background-color','", colors$color3, "')"))
    runjs(paste0("$('.gtc-button-rev:hover').css('color','", colors$color6, "').css('background-color','", colors$color8, "')"))
    runjs(paste0("$('.js-irs-0 .irs-to, .js-irs-0 .irs-from, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-1 .irs-to, .js-irs-1 .irs-from, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-2 .irs-to, .js-irs-2 .irs-from, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    
  }
  
  change_font <- function(font) {
    
    #font family change
    if (!is.null(font) && font != "") {
      runjs(paste0("$('.content-wrapper .content, .box-header-title').css('font-family','", font, "')"))
    }
  }
  
  observeEvent(input$restore_box, {
    updateBox("input_box", action = "restore")
    updateBox("weight_box", action = "restore")
    updateBox("length_box", action = "restore")
    updateBox("HC_box", action = "restore")
    if (rv$hasDisplay == 1) {
      updateBox("output_box", action = "restore")
    }
  })
  
  observeEvent(input$settings_button, {
    if (rv$setting_html[1] == "") {
      updateActionButton(session, "settings_button", label = "CLOSE SETTINGS", icon = tags$i(class="fa-solid fa-circle-xmark"))
      #hide("restore_box")
      rv$setting_html = 
        box(class = "settings-box", 
            title = span("SETTINGS", class = "box-header-title", style=paste0("color:",colors$color3,";font-family:",rv$font,";")),
            width = 12,
            collapsible = FALSE,
            closable = FALSE,
            div(align="right", 
                actionButton("save_settings", "APPLY SETTINGS", class = "gtc-button", style=paste0("color:",colors$color3,";background-color:",colors$color6,";border:0;"))),
            accordion(id = "settings_accordion",
                      accordionItem(title = span(class = "box-header-title", "Color Theme", style=paste0("color:",colors$color3,";font-family:",rv$font,";")), collapsed = FALSE,
                                    fluidRow(
                                      column(12,
                                             selectizeInput("color_theme_selection", "Pick a color theme",
                                                            choices = NULL))
                                    )),
                      accordionItem(title = span(class = "box-header-title", "System Font", style=paste0("color:",colors$color3,";font-family:",rv$font,";")), collapsed = FALSE,
                                    fluidRow(
                                      column(12,
                                             selectInput("font_selection", "Select a font", 
                                                         choices = c("Choose One..." = "", "Nunito", "Lexend", "Times New Roman", "Arial", "Archivo Narrow", "Noto Sans Display"),
                                                         selected = "")
                                      )
                                    ))
            )
            
        )
      

    } else if (class(rv$setting_html) == "shiny.tag") {
      rv$setting_html = ""
      updateActionButton(session, "settings_button", label = "OPEN SETTINGS", icon = tags$i(class="fa-solid fa-gear"))

      if (rv$hasDisplay == 1) {
        updateBox("output_box", "restore")
      }
    }
    
    id <- read_csv("../REGISTRATION/www/user.csv")
    selected_color_theme <- id$color_theme[id$id %in% input$id]
    font                 <- id$font[id$id %in% input$id]
    
    output$settings <- renderUI({rv$setting_html})
    
    updateSelectInput(
      session, "font_selection",
      selected = font
    )
    
    updateSelectizeInput(
      session, "color_theme_selection",
      selected = color_df$Theme[selected_color_theme],
      choices = color_df,
      options = list(
        render = I(
          '{
        item: function(item, escape) {
          return "<div>" + item.value + "</div>";
          },
        option: function(item, escape) {
          return "<div>" + item.value + "</div>";
          }
        }'), server = FALSE
      )
    )
    
    
  })
  
  observeEvent(input$signin, {
    id <- read_csv("../REGISTRATION/www/user.csv")
    pwd <- read_csv("../REGISTRATION/www/pwd.csv")
    
    if (!(input$id %in% id$id)) {
      output$signin_msg <- renderUI({
        div(style="color:#cc0000;font-weight:500;font-size:16px;", "Account does not exist! Please register.")
      })
      output$greeting <- renderUI({})
    } else if (input$id %in% id$id) {
      output$signin_msg <- renderUI({})
      if (!(encrypt_string(input$password, key = input$id) %in% pwd$pwd)) {
        output$signin_msg <- renderUI({
          div(style="color:#cc0000;font-weight:500;font-size:16px;", "Password incorrect. Please try again.")
        })
        output$greeting <- renderUI({})
        
      } else if (encrypt_string(input$password, key = input$id) %in% pwd$pwd) {
        output$signin_msg <- renderUI({})
        
        #display name
        name <- read_csv("../REGISTRATION/www/registration.csv") %>% 
          filter(username %in% input$id) %>% 
          select(firstName, lastName)
        full.name <- paste(name$firstName, name$lastName)
        output$greeting <- renderUI({
          div(paste0("Hi! ", full.name))
        })
        
        #load color theme
        selected_color_theme <- id$color_theme[id$id %in% input$id]
        rv$font <- id$font[id$id %in% input$id]
        change_color_palette(selected_color_theme)
        change_color_theme()
        change_font(rv$font)
        
        #load saved infant ID
        saved_infant_id <- refresh_saved_infant()
        updateTabsetPanel(session, "main", "uploader")
        
        #instructions for data table
        output$table_assignment <- renderUI({
          div(style="font-size:12px;",
              HTML(
                "
                Instructions:
                <ul>
                  <li>The data table can be in CSV or XLSX format.</li>
                  <li>When using the XLSX format, place the data table on the first worksheet.</li>
                  <li>The data table has to include ID, Sex, GA_week, GA_day, DOL columns with corresponding column names.</li>
                  <li>The WebApp is case-sensitive.</li>
                  <li>GA_week & GA_day are birth GA. Only need to appear once for each ID.</li>
               </ul>
              "
              ))
        })

      }
    }
  })
  
  observeEvent(input$upload_file, {
    
    uploadedFile <- input$upload_file
    ext <- tools::file_ext(uploadedFile$datapath)
    if (ext %in% "csv") {
      rv$uploaded_dt <- read_csv(uploadedFile$datapath)
    } else if (ext %in% "xlsx") {
      rv$uploaded_dt <- readxl::read_excel(uploadedFile$datapath)
    }
    
    if (!all(c("ID", "Sex", "GA_week", "GA_day", "DOL") %in% colnames(rv$uploaded_dt))) {
      output$msg <- renderUI({
        span(style="color:red;", "Required column or column names are missing!")
      })
      output$table_assignment <- renderUI({
        div(style="font-size:12px;",
            HTML(
              "
                Instructions:
                <ul>
                  <li>The data table can be in CSV or XLSX format.</li>
                  <li>When using the XLSX format, place the data table on the first worksheet.</li>
                  <li>The data table has to include ID, Sex, GA_week, GA_day, DOL columns with corresponding column names.</li>
                  <li>The WebApp is case-sensitive.</li>
                  <li>GA_week & GA_day are birth GA. Only need to appear once for each ID.</li>
               </ul>
              "
            ))
      })
    } else {
      output$table_assignment <- renderUI({
        div(
          selectInput("weightColumn", "Weight Column", choices = c("Choose One..." = "", colnames(rv$uploaded_dt))),
          hr(),
          selectInput("lengthColumn", "Length Column", choices = c("Choose One..." = "", colnames(rv$uploaded_dt))),
          hr(),
          selectInput("HCColumn", "Head Circumference Column", choices = c("Choose One..." = "", colnames(rv$uploaded_dt))),
          hr(),
          selectInput("groupingColumns", "Columns for Grouping (multiple)", choices = c("Choose One..." = "", colnames(rv$uploaded_dt)), multiple = TRUE),
        )
      })
      output$msg <- renderUI({})
      rv$dt <- data.frame()
      shinyjs::show("plot")
      shinyjs::show("analyzedTable")
    }
    
    reset_graph_box(session, input, output)
    
  })
  
  observeEvent(input$plot, {
    withProgress(message = "Tidying the dataset",
                 value = 0,
                 {
                   setProgress(0.1, "Checking...")
                   if (nrow(rv$uploaded_dt) == 0) {
      output$msg <- renderUI({
        span(style="color:red;", "Please upload a datatable first!")
      })
    } else if (nrow(rv$uploaded_dt) > 0) {
      output$msg <- renderUI({})
      setProgress(0.2, "Upload successfully...")
      
      if (length(input$weightColumn) == 0 | input$weightColumn == "") {
        weight = 0
      } else {
        weight = 1
      }
      
      if (length(input$lengthColumn) == 0 | input$lengthColumn == "") {
        length = 0
      } else {
        length = 1
      }
      
      if (length(input$HCColumn) == 0 | input$HCColumn == "") {
        HC = 0
      } else {
        HC = 1
      }
      setProgress(0.3, "Columns triaged...")
      
      if (weight + length + HC == 0) {

        output$msg <- renderUI({
          span(style="color:red;", "Assign at least one measurement column!")
        })
        
      } else if ((weight + length + HC) > 0) {
        setProgress(0.4, "Column selection checked...")
        if (any(input$weightColumn != "" & input$weightColumn == input$lengthColumn, 
                input$lengthColumn != "" & input$lengthColumn == input$HCColumn, 
                input$HCColumn !="" & input$HCColumn == input$weightColumn)) {
          output$msg <- renderUI({
            span(style="color:red;", paste0("One column was selected twice."))
          })
          output$meas_table_UI <- renderUI({})
        } else {
          output$msg <- renderUI({})
          checkGA <- rv$uploaded_dt %>% 
            select(ID, GA_week, GA_day) %>%
            filter(!is.na(as.numeric(GA_week)), !is.na(as.numeric(GA_day))) %>%
            unique(.) %>%
            group_by(ID) %>% summarise(n=n())
          if (any(checkGA$n > 1)) {
            IDissue <- checkGA$ID[checkGA$n > 1]
            output$msg <- renderUI({
              span(style="color:red;", paste0("More than one GA for ID ", paste0(IDissue, collapse =", "), "."))
            })
          } else if (all(checkGA$n == 1)) {
            checkSex <- rv$uploaded_dt %>% 
              mutate(Sex = ifelse(str_detect(Sex, "[Ff]"), "Female", "Male")) %>%
              select(ID, Sex) %>%
              filter(!is.na(as.character(Sex))) %>%
              unique(.) %>%
              group_by(ID) %>% summarise(n=n())
            if (any(checkSex$n > 1)) {
              IDissue <- checkSex$ID[checkSex$n > 1]
              output$msg <- renderUI({
                span(style="color:red;", paste0("More than one sex assigned to ID ", paste0(IDissue, collapse =", "), "."))
              })
            } else if (all(checkSex$n == 1)) {

                             setProgress(0.5, "Verify data type...")
                             rv$dtTidy <- rv$uploaded_dt %>%
                               mutate(
                                 ID = as.factor(ID),
                                 GA_week = as.numeric(GA_week),
                                 GA_day = as.numeric(GA_day),
                                 Sex = ifelse(str_detect(Sex, "[Ff]"), "Female", "Male"),
                                 Sex = as.factor(Sex)
                               ) %>%
                               group_by(ID) %>%
                               fill(GA_week, .direction = "downup") %>%
                               fill(GA_day, .direction = "downup") %>%
                               fill(Sex, .direction = "downup") %>%
                               filter(GA_week*7 + GA_day >= 22*7+4, GA_week*7 + GA_day <= 34*7+3, !is.na(Sex), !is.na(GA_week), !is.na(GA_day)) %>%
                               ungroup()
                             rv$uploaded_dt <- data.frame()
                             
                             setProgress(0.7, "Grouping columns...")
                             if (length(input$groupingColumns) > 0) {
                               for (i in input$groupingColumns) {
                                 rv$dtTidy <- rv$dtTidy %>% fill(starts_with(i) & ends_with(i), .direction = "downup")
                               }
                               rv$dtTidy <- rv$dtTidy %>%
                                 unite(remove = FALSE, col = "_Group", input$groupingColumns) %>%
                                 mutate(`_Group` = as.factor(`_Group`))
                             } else if (length(input$groupingColumns) == 0) {
                               rv$dtTidy <- rv$dtTidy %>% 
                                 mutate(`_Group` = "Group1",
                                        `_Group` = as.factor(`_Group`))
                             }
                             
                             setProgress(0.8, "Rearrange measurement values...")
                             if (weight == 1) {
                               rv$dtTidy$`_Weight` <- as.numeric(rv$dtTidy[[input$weightColumn]])
                             }
                             if (length == 1) {
                               rv$dtTidy$`_Length` <- as.numeric(rv$dtTidy[[input$lengthColumn]])
                             }
                             if (HC == 1) {
                               rv$dtTidy$`_HeadCircumference` <- as.numeric(rv$dtTidy[[input$HCColumn]])
                             }
                             rv$dtTidy[c(input$weightColumn, input$lengthColumn, input$HCColumn)] <- NULL
                             
                             setProgress(0.9, "Extract columns...")
                             col <- intersect(c("ID", "Sex", "GA_week", "GA_day", "DOL", "_Weight", "_Length", "_HeadCircumference", "_Group"), colnames(rv$dtTidy))
                             rv$dtTidy <- rv$dtTidy %>% select(col)
                             rv$dt <- rv$dtTidy
                             setProgress(1, "Task completed...")

              
              ### Show buttons
              shinyjs::show("includeTP")
              shinyjs::show("calculate") 
              shinyjs::show("save_dt") 
              
              ### Update graph boxes
              reset_graph_box(session, input, output)
              
              ### show table UI
              
              output$meas_table_UI <- renderUI({
                div(div(id = "recordsPerPage", style="padding-bottom:6px;", "Up to 10,000 records per page"),
                    DTOutput("meas_table"), style = "height:640px;padding-right:12px;font-size:11px;")
                
              })
            }
            
            
            
          }
        }
      }
    }
    }
              )
    
  })
  
  
  observeEvent(input$analyzedTable, {
    hide("calculate")
    hide("includeTP")
    output$msg <- renderUI({})
    
    rv$dt <- rv$uploaded_dt
    rv$dtZ <- rv$uploaded_dt
    col <- c("_Weight Percentile", "_Length Percentile", "_HeadCircumference Percentile")
    if ("_Group" %in% colnames(rv$uploaded_dt)) {
      if (any(col %in% colnames(rv$uploaded_dt))) {
        rv$dtTP <- rv$uploaded_dt %>% 
          select(any_of(c("ID", "_Group", col))) %>%
          unique(.)
        
        output$meas_table_UI <- renderUI({
          div(div(id = "recordsPerPage", style="padding-bottom:6px;", "Up to 10,000 records per page"),
              DTOutput("meas_table"), style = "height:640px;padding-right:12px;font-size:11px;")
          
        })
        
        rv$uploaded_dt <- data.frame()
        shinyjs::show("save_dt")
        
      } else {
        output$msg <-renderUI({
          span(style="color:red;", "It appears this table has not been analyzed!")
        })
      }
    } else {
      output$msg <-renderUI({
        span(style="color:red;", "It appears this table has not been analyzed!")
      })
    }
    

  })
  
  
  observeEvent(rv$dtTidy, {
    
    if (nrow(rv$dtTidy) > 0) {
      if ("_Weight" %in% colnames(rv$dtTidy)) {
        delay(nrow(rv$dtTidy)/5,
              {
                if (mean(rv$dtTidy$`_Weight`, na.rm = TRUE) < 10) {
                  showModal(
                    modalDialog("Is weight data in kilograms?",
                                "Covert to grams?",
                                footer = tagList(actionButton("convertToGrams", "CONVERT"), modalButton("CANCEL"))
                    )
                  )
                }
                
              })
      }
      
    }
  })
  
  observeEvent(input$convertToGrams, {
    
    rv$dtTidy$`_Weight` = round(1000 * rv$dtTidy$`_Weight`)
    removeModal()
    rv$dt <- rv$dtTidy
  })

  observeEvent(rv$dt, {
    
    output$meas_table <- renderDT({
      datatable(rv$dt, 
                rownames = FALSE, 
                filter = list(position = 'top', clear = TRUE),
                selection = "none", 
                extensions = "Buttons",
                callback=JS('$(\'div.has-feedback input[type="search"]\').attr( "placeholder", "" )'),
                options = list(dom = "Btpr", 
                               #paging = FALSE,
                               pageLength = 10000,
                               scrollY = "500",
                               scrollX = TRUE,
                               buttons = list(
                                 list(extend = "copy"),
                                 list(extend = "csv", filename = "nicugrowth"),
                                 list(extend = "excel", filename = "nicugrowth"),
                                 list(extend = "pdf", filename = "nicugrowth"),
                                 list(extend = "print", 
                                      autoPrint = TRUE, 
                                      title = "nicugrowth.app Batch Analysis Table Output",
                                      exportOptions = list(
                                        orientation = "landscape"
                                      ))
                               ),
                               columnDefs = list(list(className = 'dt-right', targets = "_all"))))
      
    })
  })
  
  observeEvent(input$calculate, {
    
    checkGroup <- rv$dtTidy %>% 
      select(ID, `_Group`) %>%
      filter(!is.na(as.character(`_Group`))) %>%
      unique(.) %>%
      group_by(ID) %>% summarise(n=n())
    
    #rv$dt <- checkGroup
    
    if (any(checkGroup$n > 1)) {
      IDissue <- checkGroup$ID[checkGroup$n > 1]
      output$msg <- renderUI({
        span(style="color:red;", paste0("More than one group assignment present in the data for ID ", paste0(IDissue, collapse =", "), "."), br(), "Analaysis aborted.")
      })
    } else if (all(checkGroup$n == 1)) {
      withProgress(value= 0.5, message = "Calculating Z", {
        Weight <- calculate_Z(data = rv$dtTidy, type = "Weight")
        Length <- calculate_Z(data = rv$dtTidy, type = "Length")
        HC <- calculate_Z(data = rv$dtTidy, type = "HeadCircumference")
      })
      
      col <- c("ID", "Sex", "GA_week", "GA_day", "GAn", "DOL", "Day", "_Weight", "_Weight Z", "_Length", "_Length Z", "_HeadCircumference", "_HeadCircumference Z", "_Group")
      rv$dtZ <- Weight[intersect(col, colnames(Weight))] %>% full_join(Length[intersect(col, colnames(Length))]) %>% full_join(HC[intersect(col, colnames(HC))]) 
      
      if (input$includeTP) {
        Weight_percentile <- Weight %>% FindPercentileForAll(., "Weight", 1000, colors, input$id)
        Length_percentile <- Length %>% FindPercentileForAll(., "Length", 1, colors, input$id)
        HC_percentile <- HC %>% FindPercentileForAll(., "HeadCircumference", 1, colors, input$id)
        
        rv$dtTP <- Weight_percentile %>% left_join(Length_percentile) %>% left_join(HC_percentile) %>% select(ID, Group, everything()) %>%
          rename(`_Group` = Group)
        
        rv$dtZ <- rv$dtZ %>% left_join(rv$dtTP)
      }

      rv$dt <- rv$dtZ
      
      output$msg <- renderUI({
        span(style="color:red;", "Analysis completed!!!")
      })
      
      updateBox("weight_box", action = "toggle")
      updateBox("length_box", action = "toggle")
      updateBox("HC_box", action = "toggle")
      updateBox("output_box", action = "restore")
      
      }
    

  })
  
  observeEvent(input$weight_type_analysis, {
    if ("_Weight" %in% colnames(rv$dtZ)) {
      if (input$weight_type_analysis == "1") {
        rv$dt <- rv$dtZ
        table <- rv$dtZ %>% select(ID, Sex, GA_week, GA_day, GAn, DOL, Day, `_Group`, `_Weight`, `_Weight Z`)
        output$weight_UI <- renderUI({plotOutput("weight_plot", width = paste0(30 * length(unique(table$GAn)), "%"),  height = "600px")})
        output$weight_plot <- renderPlot({plot_by_DOL(table, "Weight", colors = colors, input$font_selection)})
        
      } else if (input$weight_type_analysis == "2") {
        rv$dt <- rv$dtZ
        output$weight_UI <- renderUI({
          fluidRow(column(6, plotOutput("weight_z_plot")),
                   column(6, plotOutput("weight_z_plot_facet")))
        })
        
        plotbyZ <- plot_by_Z(rv$dtZ, "Weight", colors = colors, input$font_selection)
        
        output$weight_z_plot <- renderPlot({plotbyZ + labs(title = "Weight Z-Score by Chronological Age")})
        output$weight_z_plot_facet <- renderPlot({
          plotbyZ + facet_wrap(~`_Group`) + 
            labs(title = "Weight Z-Score by Chronological Age & by Group") + 
            theme(axis.text = element_text(size = 10))
        })
      } else if (input$weight_type_analysis == "3") {
        rv$dt <- rv$dtZ
        output$weight_UI <- renderUI({
          fluidRow(column(6, plotOutput("weight_percentile_plot")),
                   column(6, plotOutput("weight_percentile_plot_facet")))
        })
        
        plotbyPercentile <- plot_by_Percentile(rv$dtZ, "Weight", colors = colors, input$font_selection)
        
        output$weight_percentile_plot <- renderPlot({plotbyPercentile + labs(title = "Weight Percentile by Chronological Age")})
        output$weight_percentile_plot_facet <- renderPlot({
          plotbyPercentile + facet_wrap(~`_Group`) + 
            labs(title = "Weight Percentile by Chronological Age & by Group") + 
            theme(axis.text = element_text(size = 10))
        })
      } else if (input$weight_type_analysis == "4") {
        if (nrow(rv$dtTP) > 0) {
          
          rv$dt <- rv$dtTP
          content <- TP_comparison(rv$dtTP, "Weight", colors, input$font_selection)
          
          output$weight_UI <- renderUI({
            fluidRow(column(6, plotOutput("weight_TP_plot")),
                     column(6, gt_output("weight_TP_comTable")))
          })
          
          output$weight_TP_plot <- renderPlot({content[[2]]})
          output$weight_TP_comTable <- render_gt({content[[1]]}, width = "100%")
          
        } else {
          output$weight_UI <- renderUI({})
        }
        
      } else if (input$weight_type_analysis == "5") {
        content <- delta_Z_first_last(rv$dtZ, "Weight", colors, input$font_selection)
        rv$dt <- content[[1]]
        
        output$weight_UI <- renderUI({
          fluidRow(
            column(12, "Infants will only 1 measurement are excluded."),
            column(6, plotOutput("weight_Z_Score_plot")),
            column(6, gt_output("weight_Z_Score_comTable")))
        })
        
        output$weight_Z_Score_plot <- renderPlot({content[[2]]})
        output$weight_Z_Score_comTable <- render_gt({content[[3]]}, width = "100%")
        
      } else if (input$weight_type_analysis == "6") {
        
        content <- delta_Z_first_36orLast(rv$dtZ, "Weight", colors, input$font_selection)
        rv$dt <- content[[1]]
        
        output$weight_UI <- renderUI({
          fluidRow(
            column(12, "Infants will only 1 measurement are excluded."),
            column(6, plotOutput("weight_Z_Score_plot")),
            column(6, gt_output("weight_Z_Score_comTable")))
        })
        
        output$weight_Z_Score_plot <- renderPlot({content[[2]]})
        output$weight_Z_Score_comTable <- render_gt({content[[3]]}, width = "100%")
        
      } 
    }     

  })
  
  observeEvent(input$length_type_analysis, {
    if ("_Length" %in% colnames(rv$dtZ)) {
      if (input$length_type_analysis == "1") {
        rv$dt <- rv$dtZ
        table <- rv$dtZ %>% select(ID, Sex, GA_week, GA_day, GAn, DOL, Day, `_Group`, `_Length`, `_Length Z`)
        output$length_UI <- renderUI({plotOutput("length_plot", width = paste0(30 * length(unique(table$GAn)), "%"), height = "600px")})
        output$length_plot <- renderPlot({plot_by_DOL(table, "Length", colors = colors, input$font_selection)})
        
      } else if (input$length_type_analysis == "2") {
        rv$dt <- rv$dtZ
        output$length_UI <- renderUI({
          fluidRow(
            column(12, "Infants will only 1 measurement are excluded."),
            column(6, plotOutput("length_z_plot")),
            column(6, plotOutput("length_z_plot_facet")))
        })
        
        plotbyZ <- plot_by_Z(rv$dtZ, "Length", colors = colors, input$font_selection)
        
        output$length_z_plot <- renderPlot({plotbyZ + labs(title = "Length Z-Score by Chronological Age")})
        output$length_z_plot_facet <- renderPlot({
          plotbyZ + facet_wrap(~`_Group`) + 
            labs(title = "Length Z-Score by Chronological Age & by Group") + 
            theme(axis.text = element_text(size = 10))
        })
      } else if (input$length_type_analysis == "3") {
        rv$dt <- rv$dtZ
        output$length_UI <- renderUI({
          fluidRow(
            column(12, "Infants will only 1 measurement are excluded."),
            column(6, plotOutput("length_percentile_plot")),
            column(6, plotOutput("length_percentile_plot_facet")))
        })
        
        plotbyPercentile <- plot_by_Percentile(rv$dtZ, "Length", colors = colors, input$font_selection)
        
        output$length_percentile_plot <- renderPlot({plotbyPercentile + labs(title = "Length Percentile by Chronological Age")})
        output$length_percentile_plot_facet <- renderPlot({
          plotbyPercentile + facet_wrap(~`_Group`) + 
            labs(title = "Length Percentile by Chronological Age & by Group") + 
            theme(axis.text = element_text(size = 10))
        })
      } else if (input$length_type_analysis == "4") {
        if (nrow(rv$dtTP) > 0) {
          rv$dt <- rv$dtTP
          content <- TP_comparison(rv$dtTP, "Length", colors, input$font_selection)
          
          output$length_UI <- renderUI({
            fluidRow(
              column(12, "Infants will only 1 measurement are excluded."),
              column(6, plotOutput("length_TP_plot")),
              column(6, gt_output("length_TP_comTable")))
          })
          
          output$length_TP_plot <- renderPlot({content[[2]]})
          output$length_TP_comTable <- render_gt({content[[1]]}, width = "100%")          
        } else {
          output$length_UI <- ({})
        }

      } else if (input$length_type_analysis == "5") {
        
        content <- delta_Z_first_last(rv$dtZ, "Length", colors, input$font_selection)
        rv$dt <- content[[1]]
        
        output$length_UI <- renderUI({
          fluidRow(
            column(12, "Infants will only 1 measurement are excluded."),
            column(6, plotOutput("length_Z_Score_plot")),
            column(6, gt_output("length_Z_Score_comTable")))
        })
        
        output$length_Z_Score_plot <- renderPlot({content[[2]]})
        output$length_Z_Score_comTable <- render_gt({content[[3]]}, width = "100%")
        
      } else if (input$length_type_analysis == "6") {
        
        content <- delta_Z_first_36orLast(rv$dtZ, "Length", colors, input$font_selection)
        rv$dt <- content[[1]]
        
        output$length_UI <- renderUI({
          fluidRow(
            column(12, "Infants will only 1 measurement are excluded."),
            column(6, plotOutput("length_Z_Score_plot")),
            column(6, gt_output("length_Z_Score_comTable")))
        })
        
        output$length_Z_Score_plot <- renderPlot({content[[2]]})
        output$length_Z_Score_comTable <- render_gt({content[[3]]}, width = "100%")
        
      } 
    } 

  })
  
  observeEvent(input$HC_type_analysis, {
    if ("_HeadCircumference" %in% colnames(rv$dtZ)) {
      if (input$HC_type_analysis == "1") {
        rv$dt <- rv$dtZ
        table <- rv$dtZ %>% select(ID, Sex, GA_week, GA_day, GAn, DOL, Day, `_Group`, `_HeadCircumference`, `_HeadCircumference Z`)
        output$HC_UI <- renderUI({plotOutput("HC_plot", width = paste0(30 * length(unique(table$GAn)), "%"), height = "600px")})
        output$HC_plot <- renderPlot({plot_by_DOL(table, "HeadCircumference", colors = colors, input$font_selection)})
      } else if (input$HC_type_analysis == "2") {
        rv$dt <- rv$dtZ
        output$HC_UI <- renderUI({
          fluidRow(column(6, plotOutput("HC_z_plot")),
                   column(6, plotOutput("HC_z_plot_facet")))
        })
        
        plotbyZ <- plot_by_Z(rv$dtZ, "HeadCircumference", colors = colors, input$font_selection)
        
        output$HC_z_plot <- renderPlot({plotbyZ + labs(title = "Head Circumference Z-Score by Chronological Age")})
        output$HC_z_plot_facet <- renderPlot({
          plotbyZ + facet_wrap(~`_Group`) + 
            labs(title = "Head Circumference Z-Score by Chronological Age & by Group") + 
            theme(axis.text = element_text(size = 10))
        })
      } else if (input$HC_type_analysis == "3") {
        rv$dt <- rv$dtZ
        output$HC_UI <- renderUI({
          fluidRow(column(6, plotOutput("HC_percentile_plot")),
                   column(6, plotOutput("HC_percentile_plot_facet")))
        })
        
        plotbyPercentile <- plot_by_Percentile(rv$dtZ, "HeadCircumference", colors = colors, input$font_selection)
        
        output$HC_percentile_plot <- renderPlot({plotbyPercentile + labs(title = "Head Circumference Percentile by Chronological Age")})
        output$HC_percentile_plot_facet <- renderPlot({
          plotbyPercentile + facet_wrap(~`_Group`) + 
            labs(title = "Head Circumference Percentile by Chronological Age & by Group") + 
            theme(axis.text = element_text(size = 10))
        })
      } else if (input$HC_type_analysis == "4") {
        if (nrow(rv$dtTP) > 0) {
          rv$dt <- rv$dtTP
          content <- TP_comparison(rv$dtTP, "HeadCircumference", colors, input$font_selection)
          
          output$HC_UI <- renderUI({
            fluidRow(column(6, plotOutput("HC_TP_plot")),
                     column(6, gt_output("HC_TP_comTable")))
          })
          
          output$HC_TP_plot <- renderPlot({content[[2]]})
          output$HC_TP_comTable <- render_gt({content[[1]]}, width = "100%")
        } else {
          output$HC_UI <- ({})
        }

        
      } else if (input$HC_type_analysis == "5") {
        
        content <- delta_Z_first_last(rv$dtZ, "HeadCircumference", colors, input$font_selection)
        rv$dt <- content[[1]]
        
        output$HC_UI <- renderUI({
          fluidRow(column(6, plotOutput("HC_Z_Score_plot")),
                   column(6, gt_output("HC_Z_Score_comTable")))
        })
        
        output$HC_Z_Score_plot <- renderPlot({content[[2]]})
        output$HC_Z_Score_comTable <- render_gt({content[[3]]}, width = "100%")
        
      } else if (input$HC_type_analysis == "6") {
        
        content <- delta_Z_first_36orLast(rv$dtZ, "HeadCircumference", colors, input$font_selection)
        rv$dt <- content[[1]]
        
        output$HC_UI <- renderUI({
          fluidRow(
            column(6, plotOutput("HC_Z_Score_plot")),
            column(6, gt_output("HC_Z_Score_comTable")))
        })
        
        output$HC_Z_Score_plot <- renderPlot({content[[2]]})
        output$HC_Z_Score_comTable <- render_gt({content[[3]]}, width = "100%")
        
      } 
    } 
  })
  

  observeEvent(input$color_theme_selection, {
    selected_color_theme <- gsub("Option ", "", str_extract(input$color_theme_selection, "Option\\s[0-9]+"))
    
    change_color_palette(selected_color_theme)
    
  })
  
  observeEvent(input$save_settings, {
    
    change_color_theme()
    change_font(input$font_selection)
    rv$font <- input$font_selection
    # save color theme
    selected_color_theme <- gsub("Option ", "", str_extract(input$color_theme_selection, "Option\\s[0-9]+"))
    id <- read_csv("../REGISTRATION/www/user.csv")
    id[id$id %in% input$id, "color_theme"] <- as.numeric(selected_color_theme)
    id[id$id %in% input$id, "font"] <- input$font_selection
    write_csv(id, "../REGISTRATION/www/user.csv")
    
  })
  
  output$save_dt <- downloadHandler(
    filename = function() {
      "nicugrowth.csv"
    },
    content = function(file) {
      write.csv(rv$dt, file)
    }
  )
  

  
  

  

  

  
  
  
})
