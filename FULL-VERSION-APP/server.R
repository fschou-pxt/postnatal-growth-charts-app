library(shiny)
library(duckdb)
library(DBI)
library(tidyverse)
library(DT)
library(shinyjs)
library(safer)
library(plotly)
library(patchwork)
library(colourpicker)
library(colorspace)
 
shinyServer(function(input, output, session) {
  
  type <<- NA
  
  color_palette <- read_csv("../REGISTRATION/www/color_palette.csv") %>% t(.) %>% as.data.frame() %>% rownames_to_column(var = "color_order")
  
  runjs('document.getElementById("main_frame").scrollIntoView({block:"start", behavior:"smooth"})')  
  
  rv <- reactiveValues(Chou_weight = data.frame(), 
                       dt = data.frame(),
                       proxy = data.frame(),
                       offset = NA,
                       percentile_min = NA,
                       saved_infant_id = NA,
                       hasDisplay = 0,
                       setting_html = "",
                       plot_width_x = as.numeric(NA),
                       plot_heigth = as.numeric(NA),
                       layout = "Plate",
                       output_table_format = as.character(NA),
                       font = "",
                       all_tp = NA,
                       last_seven_tp = NA,
                       lastThree = NA,
                       assessing_weight = 0,
                       signed_in = 0,
                       color_palette = read_csv("../REGISTRATION/www/color_palette.csv") %>% t(.) %>% as.data.frame() %>% rownames_to_column(var = "color_order"),
                       color_df = data.frame(Theme = character(0))
                       
  )
  observe({
    for (i in c(1:(ncol(rv$color_palette)-1))) {
      rv$color_df[i,"Theme"] <- paste0("<span style='font-size:14px;'>Option ",i,"&nbsp;&nbsp;&nbsp", paste0("<span><i class='fa-solid fa-square' style=color:", rv$color_palette[[paste0("V",i)]], ";></i></span>", collapse = ""),"</span>")
    }
  })

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
    #output$meas_table <- renderUI({})
    output$weight_plot <- renderPlotly({})
    output$length_plot <- renderPlotly({})
    output$HC_plot     <- renderPlotly({})
    updateBox("weight_box", action = "toggle")
    updateBox("length_box", action = "toggle")
    updateBox("HC_box", action = "toggle")
    updateBox("output_box", action = "toggle")
    updateSelectInput(session, "GA_week", selected = "")
    updateSelectInput(session, "GA_day", selected = "")
    updateSelectInput(session, "sex", selected = "")
    updateSliderInput(session, "weight_range", min = 0, max = 308, value = c(0, 308))
    updateSliderInput(session, "length_range", min = 0, max = 308, value = c(0, 308))
    updateSliderInput(session, "HC_range", min = 0, max = 308, value = c(0, 308))
    
    # disable("GA_week")
    # disable("GA_day")
    # disable("sex")
    rv$hasDisplay = 0 
    
    
    output$weight_trajectory_percentile <- renderUI({})
    output$weight_WHO_transition <- renderUI({})
    output$rate_acceleration <- renderUI({})
    output$rate_stable <- renderUI({})
    output$length_trajectory_percentile <- renderUI({})
    output$length_WHO_transition <- renderUI({})
    output$HC_trajectory_percentile <- renderUI({})
    output$HC_WHO_transition <- renderUI({})
  }
  
  refresh_saved_infant <- function() {
    con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
    saved_infant_id <- dbGetQuery(con, paste0("SELECT ID FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'"))
    saved_infant_id <- saved_infant_id$ID
    if (input$admitted) {
      admitted_id <- dbGetQuery(con, paste0("SELECT ID FROM \"Discharged_tbl\" WHERE USERID = '", input$id, "' AND DISCHARGED = 0"))
      admitted_id <- admitted_id$ID
      saved_infant_id <- intersect(saved_infant_id, admitted_id)
    }
    dbDisconnect(con, shutdown = TRUE)
    return(saved_infant_id)
  }
  
  change_color_palette <- function(selected_color_theme) {
    colors$color1 <- rv$color_palette[1, paste0("V",selected_color_theme)]
    colors$color2 <- rv$color_palette[2, paste0("V",selected_color_theme)]
    colors$color3 <- rv$color_palette[3, paste0("V",selected_color_theme)]
    colors$color4 <- rv$color_palette[4, paste0("V",selected_color_theme)]
    colors$color5 <- rv$color_palette[5, paste0("V",selected_color_theme)]
    colors$color6 <- rv$color_palette[6, paste0("V",selected_color_theme)]
    colors$color7 <- rv$color_palette[7, paste0("V",selected_color_theme)]
    colors$color8 <- rv$color_palette[8, paste0("V",selected_color_theme)]
    colors$color9 <- rv$color_palette[9, paste0("V",selected_color_theme)]
  }
  
  update_color_input <- function(selected_color_theme) {
    updateColourInput(session, "color1", value = rv$color_palette[1, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color2", value = rv$color_palette[2, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color3", value = rv$color_palette[3, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color4", value = rv$color_palette[4, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color5", value = rv$color_palette[5, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color6", value = rv$color_palette[6, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color7", value = rv$color_palette[7, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color8", value = rv$color_palette[8, paste0("V",selected_color_theme)], showColour = "both")
    updateColourInput(session, "color9", value = rv$color_palette[9, paste0("V",selected_color_theme)], showColour = "both")
    
  }
  
  change_color_theme <- function() {
    runjs(paste0("$('.skin-blue .main-sidebar').css('background-color','", colors$color3, "')"))
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
    runjs(paste0("$('.gtc-button-2').css('color','", colors$color3, "').css('background-color','", colors$color8, "').css('border-color','", colors$color9, "')"))
    runjs(paste0("$('.gtc-button:hover').css('color','", colors$color8, "').css('background-color','", colors$color3, "')"))
    runjs(paste0("$('.gtc-button-rev').css('color','", colors$color8, "').css('background-color','", colors$color1, "')"))
    runjs(paste0("$('.gtc-button-rev:hover').css('color','", colors$color6, "').css('background-color','", colors$color1, "')"))
    runjs(paste0("$('.js-irs-0 .irs-to, .js-irs-0 .irs-from, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-1 .irs-to, .js-irs-1 .irs-from, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-2 .irs-to, .js-irs-2 .irs-from, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    runjs(paste0("$('.js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar').css('background-color','", colors$color3, "').css('border-color','", colors$color3, "')"))
    
  }
  
  change_font <- function(font) {
    
    #font family change
    if (!is.null(font) && font != "") {
      runjs(paste0("$('.content-wrapper .content, .box-header-title').css('font-family','", font, "')"))
    }
  }
  
  reset_weight_assessment <- function() {
    rv$lastThree <- NA
    rv$last_seven_tp <- NA
    rv$all_tp <- NA
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
  
  observeEvent(rv$signed_in, {
    
    if (rv$signed_in == 1) {
      shinyjs::show("settings_button")
    } else if (rv$signed_in == 0) {
      shinyjs::hide("settings_button")
      output$greeting <- renderUI({})
      
    }

  })
  
  observeEvent(rv$hasDisplay, {
    if (rv$hasDisplay == 1) {
      disable("plot")
    } else if (rv$hasDisplay == 0) {
      enable("plot")
    }
  })
  
  observeEvent(input$settings_button, {
    if (rv$setting_html[1] == "") {
      updateActionButton(session, "settings_button", label = "", icon = tags$i(class="fa-solid fa-circle-xmark"))
      #hide("restore_box")
      rv$setting_html <- 
        div(
        box(class = "settings-box", 
            title = span("SETTINGS", class = "box-header-title", style=paste0("color:",colors$color3,";font-family:",rv$font,";")),
            width = 12,
            collapsible = FALSE,
            closable = FALSE,
            fluidRow(
              column(4,
                     selectizeInput("color_theme_selection", "Pick a color theme",
                                    choices = NULL)),
              column(4,
                     selectInput("font_selection", "Select a font", 
                                 choices = c("Choose One..." = "", "Nunito", "Lexend", "Times New Roman", "Arial", "Archivo Narrow", "Noto Sans Display"),
                                 selected = "")),
              column(4,
                     div(align="left", 
                         actionButton("save_settings", "REPLACE THEME", class = "gtc-button large-padding", style=paste0("padding:7px 10px 7px 10px;margin-top:25px;color:", colors$color3, ";background-color:", colors$color6, ";")))),
              ),
            hr(),
            fluidRow(style="border:1px solid black;border-radius:5px;padding:6px 0 6px 0;",
              div(class="col-lg-1 col-md-2 col-sm-4",
                         colourInput("color1", "Color 1", colors$color1)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color2", "Color 2", colors$color2)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color3", "Color 3", colors$color3)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color4", "Color 4", colors$color4)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color5", "Color 5", colors$color5)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color6", "Color 6", colors$color6)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color7", "Color 7", colors$color7)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color8", "Color 8", colors$color8)),
              div(class="col-lg-1 col-md-2 col-sm-4",
                  colourInput("color9", "Color 9", colors$color9)),
              div(class="col-lg-3 col-md-6 col-sm-12", align="right",
                  actionButton("testTheme", "TEST THEME", class = "gtc-button-2 large-padding", style=paste0("padding:7px 10px 7px 10px;margin-top:25px;margin-right:15px;color:", colors$color3, ";background-color:", colors$color8, ";border:1px solid ",colors$color9,";")),
                  actionButton("newTheme", "ADD THEME", class = "gtc-button-2 large-padding", style=paste0("padding:7px 10px 7px 10px;margin-top:25px;margin-right:15px;color:", colors$color3, ";background-color:", colors$color8, ";border:1px solid ",colors$color9,";")))
              
            )
            
        ))
      
      id <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
      selected_color_theme <- id$color_theme[id$id %in% input$id]
      font                 <- id$font[id$id %in% input$id]
      
      updateSelectInput(
        session, "font_selection",
        selected = font
      )
      
      updateSelectizeInput(
        session, "color_theme_selection",
        selected = rv$color_df$Theme[selected_color_theme],
        choices = rv$color_df,
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

    } else if (class(rv$setting_html) == "shiny.tag") {
      rv$setting_html <- ""
      updateActionButton(session, "settings_button", label = "", icon = tags$i(class="fa-solid fa-gear"))
      
      # if (rv$hasDisplay == 1) {
      #   updateBox("output_box", "restore")
      # }
    }
    output$settings <- renderUI({rv$setting_html})
    })
  
  observeEvent(input$testTheme, {
    colors$color1 <- input$color1
    colors$color2 <- input$color2
    colors$color3 <- input$color3
    colors$color4 <- input$color4
    colors$color5 <- input$color5
    colors$color6 <- input$color6
    colors$color7 <- input$color7
    colors$color8 <- input$color8
    colors$color9 <- input$color9
    change_color_theme()
    
  })
  
  updateTheme <-function(row) {
    newTheme <- c(input$color1, input$color2, input$color3, input$color4, input$color5, input$color6, input$color7, input$color8, input$color9)
    rv$color_palette[[paste0("V", row)]] <- newTheme
    tbl <- rv$color_palette %>% column_to_rownames(var = "color_order") %>% t(.) %>% as.data.frame(.)
    rownames(tbl) <- NULL
    write_csv(tbl, "../REGISTRATION/www/color_palette.csv")
    
    for (i in c(1:(row))) {
      rv$color_df[i,"Theme"] <- paste0("<span style='font-size:14px;'>Option ",i,"&nbsp;&nbsp;&nbsp", paste0("<span><i class='fa-solid fa-square' style=color:", rv$color_palette[[paste0("V",i)]], ";></i></span>", collapse = ""),"</span>")
    }
    
    updateSelectizeInput(
      session, "color_theme_selection",
      selected = rv$color_df$Theme[row],
      choices = rv$color_df,
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
  }
  
  observeEvent(input$save_settings, {
    selected_color_theme <- as.numeric(gsub("Option ", "", str_extract(input$color_theme_selection, "Option\\s[0-9]+")))
    updateTheme(selected_color_theme)
    change_color_palette(selected_color_theme)
    change_color_theme()
    change_font(input$font_selection)
    rv$font <- input$font_selection
    # save color theme
    id <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
    id[id$id %in% input$id, "color_theme"] <- selected_color_theme
    id[id$id %in% input$id, "font"] <- input$font_selection
    write_csv(id, "~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
    
  })
  
  observeEvent(input$newTheme, {
    updateTheme(ncol(rv$color_palette)-1)
  })
  
  observeEvent(input$signin, {
    output$google_sites <- renderUI({})
    
    id <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/user.csv")
    pwd <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/pwd.csv")
    
    if (!(input$id %in% id$id)) {
      output$signin_msg <- renderUI({
        div(style="color:#cc0000;font-weight:500;font-size:16px;", "Account does not exist! Please register.")
      })
      output$greeting <- renderUI({div("Welcome to nicugrowth.app")})
    } else if (input$id %in% id$id) {
      output$signin_msg <- renderUI({})
      if (!(encrypt_string(input$password, key = input$id) %in% pwd$pwd)) {
        output$signin_msg <- renderUI({
          div(style="color:#cc0000;font-weight:500;font-size:16px;", "Password incorrect. Please try again.")
        })
        output$greeting <- renderUI({div("Welcome to nicugrowth.app")})
        
      } else if (encrypt_string(input$password, key = input$id) %in% pwd$pwd) {
        output$signin_msg <- renderUI({})
        rv$signed_in <- 1
        #display name
        name <- read_csv("~/Apps/GTC-Website-Apps/GTC-Website-Registration/www/registration.csv") %>% 
          filter(username %in% input$id) %>% 
          select(firstName, lastName)
        full.name <- paste(name$firstName, name$lastName)
        output$greeting <- renderUI({
          div(paste0("Hi! ", full.name))
        })
        
        #load color theme
        selected_color_theme <- id$color_theme[id$id %in% input$id]
        rv$font <- id$font[id$id %in% input$id]
        update_color_input(selected_color_theme)
        change_color_palette(selected_color_theme)
        change_color_theme()
        change_font(rv$font)
        
        #load saved infant ID
        saved_infant_id <- refresh_saved_infant()
        output$id_selector <- renderUI({
          selectInput("id_dropdown", "Select an Infant", choices = c(list("Select an ID..." = list(""), "Exising ID" = as.list(saved_infant_id), "---" = list("CREATE A NEW ID...", "---"))))
        })
        
        updateTabsetPanel(session, "main", "selector")
        runjs('document.getElementById("main_frame").scrollIntoView({block:"start", behavior:"smooth"})')  
        
      }
    }
  })
  
  observeEvent(input$id_dropdown, {
    saved_infant_id <- refresh_saved_infant()
    
    if (input$id_dropdown == "CREATE A NEW ID...") {
      showModal(
        modalDialog(
          title = span("CREATE A NEW ID", style=paste0("font-family:", rv$font, ";color:", colors$color2, ";")),
          div(style=paste0("font-family:", rv$font, ";color:", colors$color2, ";"),
          textInput("new_id", "Enter an ID"),
          uiOutput("new_id_msg")),
          footer = list(actionButton("create_id", span("CREATE", style=paste0("font-family:", rv$font, ";")), style=paste0("border:0;color:", colors$color3, ";background-color:",colors$color6)), 
                        actionButton("cancel_id", span("CANCEL", style=paste0("font-family:", rv$font, ";")), style=paste0("border:0;color:", colors$color3, ";background-color:",colors$color6)))
        )
      )
    } else if (input$id_dropdown %in% saved_infant_id) {
      if (rv$hasDisplay == 1) {
        restore()

      }
      
      con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
      temp_demographics <- dbGetQuery(con, paste0("SELECT * FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
      discharged <- dbGetQuery(con, paste0("SELECT DISCHARGED FROM \"Discharged_tbl\" WHERE USERID = '", input$id, "' AND ID = '", input$id_dropdown, "';"))
      dbDisconnect(con, shutdown = TRUE)
      updateSelectInput(session, "GA_week", selected = temp_demographics$GA_week[1])
      updateSelectInput(session, "GA_day", selected = temp_demographics$GA_day[1])
      updateSelectInput(session, "sex", selected = temp_demographics$Sex[1])
      disable("GA_week")
      disable("GA_day")
      disable("sex")
      updateCheckboxInput(session, "discharged", value = ifelse(discharged %in% c(1, "1"), TRUE, FALSE))
    } else if (!(input$id_dropdown %in% saved_infant_id) & input$id_dropdown != "") {
      if (rv$hasDisplay == 1) {
        restore()
      }
      updateSelectInput(session, "GA_week", selected = "")
      updateSelectInput(session, "GA_day", selected = "")
      updateSelectInput(session, "sex", selected = "")
      enable("GA_week")
      enable("GA_day")
      enable("sex")
      disable("save_data")
      disable("calculate")
      disable("weight_assess")

    }
    change_color_theme()
    
  })

  
  observeEvent(input$create_id, {
    saved_infant_id <- refresh_saved_infant()
    if (input$new_id %in% saved_infant_id) {
      output$new_id_msg <- renderUI({
        span(style="color:red;", "ID already existed!")
      })
    } else {
      output$new_id_msg <- renderUI({})
      updateSelectInput(session, "id_dropdown", 
                        choices = c(list("Exising ID" = as.list("Select an ID..." = list(""), c(saved_infant_id, input$new_id)), "---" = list("CREATE A NEW ID...", "---"))),
                        selected = input$new_id)
      updateCheckboxInput(session, "discharged", value = FALSE)
      removeModal()
    }
    
  })
  
  observeEvent(input$cancel_id, {
    removeModal()
    restore()
    saved_infant_id <- refresh_saved_infant()
    updateSelectInput(session, "id_dropdown", 
                      choices = c(list("Exising ID" = as.list("Select an ID..." = list(""), c(saved_infant_id)), "---" = list("CREATE A NEW ID...", "---"))),
                      selected = "")
    
  })
  
  observeEvent(input$discharged, {
    con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
    dbExecute(con, paste0("UPDATE Discharged_tbl SET DISCHARGED = ", ifelse(input$discharged, 1, 0), "WHERE ID = '", input$id_dropdown, "' AND USERID = '", input$id, "';"))
    dbDisconnect(con, shutdown = TRUE)
  })
  
  observeEvent(input$save_data, {
    saved_infant_id <- refresh_saved_infant()
    
    if (input$id_dropdown %in% saved_infant_id) {
      con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
      dbExecute(con, paste0("DELETE FROM \"meas_table_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
      dbAppendTable(con, "meas_table_fullApp", rv$proxy %>% 
                      mutate(USERID = input$id, ID = input$id_dropdown) %>% 
                      select(USERID, ID, DOL, PMA, Weight, Length, HC))
      dbDisconnect(con, shutdown = TRUE)
      
    } 
    showModal(
      modalDialog(
        title = span("SAVE DATA", style=paste0("font-family:", rv$font, ";color:", colors$color2, ";")),
        footer = actionButton("close_btn_save_data", span("CLOSE", style=paste0("font-family:", rv$font,";")), style=paste0("border:0;border:0;color:", colors$color3, ";background-color:",colors$color6)),
        h5(span(tags$i(class="fa-regular fa-circle-check"), "Data table has been successfully saved to the database."), style=paste0("color:",colors$color3,";font-size:16px;font-family:",rv$font, ";"))
      )
    )
    
  })
  
  observeEvent(input$close_btn_save_data, {
    removeModal()
  })
  
  observeEvent(input$delete_record, {
    showModal(
      modalDialog(
        title = span("DELETE RECORD", style=paste0("font-family:", rv$font, ";color:", colors$color2, ";")),
        footer = list(actionButton("confirm_delete", span("CONFIRM", style=paste0("font-family:", rv$font,";")), style=paste0("border:0;color:", colors$color3, ";background-color:",colors$color6)), 
                      actionButton("cancel_btn_delete_record", span("CANCEL", style=paste0("font-family:", rv$font,";")), style=paste0("border:0;color:", colors$color3, ";background-color:",colors$color6))
                      ),
        h5("Are you sure you want to delete all records pertaining to ID: ", strong(input$id_dropdown), "?", style=paste0("color:",colors$color9,";font-size:16px;font-family:", rv$font,";"))
      )
    )
  })
  
  observeEvent(input$cancel_btn_delete_record, {
    removeModal()
  })
  
  observeEvent(input$confirm_delete, {
    removeModal()
    con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
    dbExecute(con, paste0("DELETE FROM \"demographics_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
    dbExecute(con, paste0("DELETE FROM \"meas_table_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';"))
    dbDisconnect(con, shutdown = TRUE)
    
    saved_infant_id <- refresh_saved_infant()
    updateSelectInput(session, "id_dropdown", 
                      choices = c(list("Select an ID..." = list(""), "Exising ID" = as.list(saved_infant_id), "---" = list("CREATE A NEW ID...", "---"))),
                      selected = "")
    if (rv$hasDisplay == 1) {
      restore()
    }
    
  })
  
  observeEvent(input$plot, {
    
    if (rv$hasDisplay == 1) {
      restore()
    }
    
    GAdays <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) #calculate GA in days
    GAn <- round(GAdays/7)
    rv$offset <- GAdays - (GAn * 7)
    
    output$msg <- renderUI({})
    
    
    if (is.na(GAn)) {
      output$msg <- renderUI({
        div(class = "msg-box", "Please select/create an ID or enter GA!", style="color:red;")
      })
      #output$meas_table <- renderUI({})
    } else if (GAn<=22 | GAn>=35) {
      output$msg <- renderUI({
        div(class = "msg-box", "GA Out of Range!", style="color:red;")
      })
      #output$meas_table <- renderUI({})
    } else if (GAn>22 & GAn<35) {
      
      if (input$sex == "") {
        output$msg <- renderUI({
          div(class = "msg-box", "Please enter infant sex!", style="color:red;")
        })
        #output$meas_table <- renderUI({})
        
      } else if (input$sex %in% c("Male", "Female")) {
        rv$hasDisplay = 1
        runjs('document.getElementById("meas_table_view").scrollIntoView(true)')  
        
        withProgress(message = "Retrieve datatable and plotting...", value = 0, {
          

        ### retrieve data
        con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
        rv$Chou_weight <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_Weight")) %>% mutate(percentile = as.numeric(NA))
        rv$Chou_length <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_Length")) %>% mutate(percentile = as.numeric(NA))
        rv$Chou_HC     <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_HeadCircumference")) %>% mutate(percentile = as.numeric(NA))
        rv$WHO_weight  <- dbGetQuery(con, paste0("SELECT * FROM WHO_Weight_",req(input$sex),"_Agemos0"))
        rv$WHO_length  <- dbGetQuery(con, paste0("SELECT * FROM WHO_Length_",req(input$sex),"_Agemos0"))
        rv$WHO_HC      <- dbGetQuery(con, paste0("SELECT * FROM WHO_HC_",req(input$sex),"_Agemos0"))
        dbDisconnect(con, shutdown = TRUE)
        
        incProgress(0.2, detail = "Datatable retrieved")
        
        saved_infant_id <- refresh_saved_infant()
        if (input$id_dropdown %in% saved_infant_id) {
          con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
          rv$dt   <- dbGetQuery(con, paste0("SELECT * FROM \"meas_table_fullApp\" WHERE USERID = '", input$id, "'AND ID = '", input$id_dropdown, "';")) %>% 
            select(-USERID, -ID) %>% 
            mutate(DOL = as.numeric(DOL),
                   PMA = as.numeric(PMA),
                   Weight = as.numeric(Weight),
                   Weight_Z = NA,
                   Length = as.numeric(Length),
                   Length_Z = NA,
                   HC = as.numeric(HC),
                   HC_Z = NA)  %>%
            mutate(Week = PMA %/% 7, Day = PMA %% 7) %>%
            arrange(DOL)
          dbDisconnect(con, shutdown = TRUE)
          rv$proxy <- rv$dt
          
        } else {
          rv$dt <-
            data.frame(
              DOL = c(0:(44*7-GAdays+rv$offset)),
              PMA = c((GAdays):(44*7+rv$offset)),
              Weight = NA,
              Weight_Z = NA, 
              Length = NA, 
              Length_Z = NA, 
              HC = NA, 
              HC_Z = NA
            ) %>%
            mutate(Week = PMA %/% 7, Day = PMA %% 7)
          demographic_temp <- data.frame(
            USERID = input$id,
            ID = input$id_dropdown,
            GA_week = input$GA_week,
            GA_day = input$GA_day,
            Sex = input$sex
          )
          discharged_temp <- data.frame(
            ID = input$id_dropdown,
            USERID = input$id,
            DISCHARGED = 0
          )
          rv$proxy <- rv$dt
          con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=FALSE)
          dbAppendTable(con, "demographics_fullApp", demographic_temp)
          dbAppendTable(con, "meas_table_fullApp", rv$proxy %>% 
                          mutate(USERID = input$id, ID = input$id_dropdown) %>% 
                          select(USERID, ID, DOL, PMA, Weight, Length, HC))
          dbAppendTable(con, "Discharged_tbl", discharged_temp)
          dbDisconnect(con, shutdown = TRUE)
        }
        

        if (input$edit_mode) {
          editable_table <- list(target = "cell", disable = list(columns = c(0,1,2,4,6,8)))
        } else {
          editable_table <- FALSE
        }
        
        output$meas_table <- renderDT({
          datatable(rv$dt %>% select(DOL, Week, Day, Weight, Weight_Z, Length, Length_Z, HC, HC_Z), 
                    rownames = FALSE, 
                    selection = "none", 
                    editable = editable_table,
                    options = list(dom = "t", 
                                   paging = FALSE,
                                   scrollY = "400",
                                   scrollX = TRUE,
                                   columnDefs = list(list(className = 'dt-right', targets = "_all"),
                                                     list(width = "7%", targets = c(0,1,2,3,5,7)))))
        })
        
        incProgress(0.6, detail = "Render table for display")
        
        
        updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c(0, max(rv$proxy$DOL)))
        updateSliderInput(session, "length_range", min = 0, max = max(rv$proxy$DOL), value = c(0, max(rv$proxy$DOL)))
        updateSliderInput(session, "HC_range", min = 0, max = max(rv$proxy$DOL), value = c(0, max(rv$proxy$DOL)))
        
        output$weight_plot <- renderPlotly({
          plots$weight_plot <-
            ggplot(rv$Chou_weight, aes(x = ((Day + rv$offset)/7))) +
            geom_line(aes(y = percentile_3_var * 1000), linewidth = 0.75, color = colors$color3, linetype = "solid") +
            geom_line(aes(y = percentile_10_var * 1000), linewidth = 0.5, color = colors$color3, linetype = "dashed") +
            geom_line(aes(y = percentile_25_var * 1000), linewidth = 0.5, color = colors$color3, linetype = "dotted") +
            geom_line(aes(y = percentile_50_var * 1000), linewidth = 0.5, color = colors$color3, linetype = "solid") +
            geom_line(aes(y = percentile_75_var * 1000), linewidth = 0.5, color = colors$color3, linetype = "dotted") +
            geom_line(aes(y = percentile_90_var * 1000), linewidth = 0.5, color = colors$color3, linetype = "dashed") +
            geom_line(aes(y = percentile_97_var * 1000), linewidth = 0.75, color = colors$color3, linetype = "solid") +
            geom_point(data = rv$proxy %>% filter(!is.na(Weight)) %>% mutate(AGE = paste0(Week,"w",Day, "d")), aes(x = PMA/7, y = Weight, PMA = AGE, Z = Weight_Z, DOL = DOL), color = colors$color9, size = 1.5) +
            geom_line(data = rv$Chou_weight %>% filter(Daily_DSB >= as.numeric(req(input$weight_range[2]))), 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile * 1000), linewidth = 0.75, color = darken(colors$color1, amount = 0.2), linetype = "dashed") +
            geom_line(data = rv$Chou_weight %>% filter(Daily_DSB %in% c((as.numeric(req(input$weight_range[1]))):(as.numeric(req(input$weight_range[2]))))), 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile * 1000), linewidth = 0.75, color = colors$color1) +
            
            labs(x = "Postmenstrual Age (week)", 
                 y = "Weight (gram)",
                 title = NULL) + 
            scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
            scale_y_continuous(breaks = 500 * c(0:20)) +
            theme_bw() +
            theme(axis.text.x = element_text(size = 8, color = "black", family = input$font_selection),
                  axis.text.y = element_text(size = 8, color = "black", family = input$font_selection, margin = margin(0, 0.1, 0, 0, "inch")),
                  axis.title = element_text(size = 10, family = input$font_selection),
                  plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"))         
          ggplotly(tooltip = c("PMA", "DOL", "Weight", "Z"),
                   plots$weight_plot
          )
        })
        
        incProgress(0.7, detail = "Plotting weight data")
        
        
        output$length_plot <- renderPlotly({
          plots$length_plot <-
            
            ggplot(rv$Chou_length, aes(x = ((Day + rv$offset)/7))) +
            geom_line(aes(y = percentile_3_var), linewidth = 0.75, color = colors$color3, linetype = "solid") +
            geom_line(aes(y = percentile_10_var), linewidth = 0.5, color = colors$color3, linetype = "dashed") +
            geom_line(aes(y = percentile_25_var), linewidth = 0.5, color = colors$color3, linetype = "dotted") +
            geom_line(aes(y = percentile_50_var), linewidth = 0.5, color = colors$color3, linetype = "solid") +
            geom_line(aes(y = percentile_75_var), linewidth = 0.5, color = colors$color3, linetype = "dotted") +
            geom_line(aes(y = percentile_90_var), linewidth = 0.5, color = colors$color3, linetype = "dashed") +
            geom_line(aes(y = percentile_97_var), linewidth = 0.75, color = colors$color3, linetype = "solid") +
            geom_point(data = rv$proxy %>% filter(!is.na(Length)) %>% mutate(AGE = paste0(Week,"w",Day, "d")), aes(x = PMA/7, y = Length, PMA = AGE, Z = Length_Z, DOL = DOL), color = colors$color9, size = 1.5) +
            geom_line(data = rv$Chou_length %>% filter(Daily_DSB >= as.numeric(req(input$length_range[2]))), 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile), linewidth = 0.75, color = darken(colors$color1, amount = 0.2), linetype = "dashed") +
            geom_line(data = rv$Chou_length %>% filter(Daily_DSB %in% c((as.numeric(req(input$length_range[1]))):(as.numeric(req(input$length_range[2]))))), 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile), linewidth = 0.75, color = colors$color1) +
            labs(x = "Postmenstrual Age (week)", 
                 y = "Length (cm)",
                 title = NULL) + 
            scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
            scale_y_continuous(breaks = 2 * c(0:50)) +
            theme_bw() +
            theme(axis.text.x = element_text(size = 8, color = "black", family = input$font_selection),
                  axis.text.y = element_text(size = 8, color = "black", family = input$font_selection, margin = margin(0, 0.1, 0, 0, "inch")),
                  axis.title = element_text(size = 10, family = input$font_selection),
                  plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"))         
          ggplotly(tooltip = c("PMA", "DOL", "Length", "Z"),
                   plots$length_plot)
        })
        
        incProgress(0.8, detail = "Plotting length data")
        
        
        output$HC_plot <- renderPlotly({
          plots$HC_plot <-
            ggplot(rv$Chou_HC, aes(x = ((Day + rv$offset)/7))) +
            geom_line(aes(y = percentile_3_var), linewidth = 0.75, color = colors$color3, linetype = "solid") +
            geom_line(aes(y = percentile_10_var), linewidth = 0.5, color = colors$color3, linetype = "dashed") +
            geom_line(aes(y = percentile_25_var), linewidth = 0.5, color = colors$color3, linetype = "dotted") +
            geom_line(aes(y = percentile_50_var), linewidth = 0.5, color = colors$color3, linetype = "solid") +
            geom_line(aes(y = percentile_75_var), linewidth = 0.5, color = colors$color3, linetype = "dotted") +
            geom_line(aes(y = percentile_90_var), linewidth = 0.5, color = colors$color3, linetype = "dashed") +
            geom_line(aes(y = percentile_97_var), linewidth = 0.75, color = colors$color3, linetype = "solid") +
            geom_point(data = rv$proxy %>% filter(!is.na(HC)) %>% mutate(AGE = paste0(Week,"w",Day, "d")), aes(x = PMA/7, y = HC, PMA = AGE, Z = HC_Z, DOL = DOL), color = colors$color9, size = 1.5) +
            geom_line(data = rv$Chou_HC %>% filter(Daily_DSB >= as.numeric(req(input$HC_range[2]))), 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile), linewidth = 0.75, color = darken(colors$color1, amount = 0.2), linetype = "dashed") +
            geom_line(data = rv$Chou_HC %>% filter(Daily_DSB %in% c((as.numeric(req(input$HC_range[1]))):(as.numeric(req(input$HC_range[2]))))), 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile), linewidth = 0.75, color = colors$color1) +
            labs(x = "Postmenstrual Age (week)",
                 y = "Head Circumference (cm)",
                 title = NULL) + 
            scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
            scale_y_continuous(breaks = 2 * c(0:50)) +
            theme_bw() +
            theme(axis.text.x = element_text(size = 8, color = "black", family = input$font_selection),
                  axis.text.y = element_text(size = 8, color = "black", family = input$font_selection, margin = margin(0, 0.1, 0, 0, "inch")),
                  axis.title = element_text(size = 10, family = input$font_selection),
                  plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"))         
          ggplotly(tooltip = c("PMA", "DOL", "HC", "Z"),
                   plots$HC_plot)
        })
        
        incProgress(0.9, detail = "Plotting head circumference data")
        
        
        updateBox("weight_box", action = "toggle")
        updateBox("length_box", action = "toggle")
        updateBox("HC_box", action = "toggle")
        #updateBox("output_box", action = "toggle")
        
        ### Show buttons
        shinyjs::show("delete_record")
        shinyjs::show("save_data")
        shinyjs::show("calculate") 
        shinyjs::show("discharged")
        shinyjs::show("weight_assess")
        
        reset_weight_assessment()
        
        incProgress(1, detail = "Done")
        
        })
      }
      
    } 
    
  })
  
  observeEvent(input$weight_range, {
    click("calculate")
    change_color_theme()
    range <- input$weight_range
    if (range[1] == 0 & range[2] == max(rv$proxy$DOL)) {
      disable("all")
    } else {
      enable("all")
    }
  })
  
  observeEvent(input$length_range, {
    click("calculate")
    change_color_theme()
    range <- input$length_range
    if (range[1] == 0 & range[2] == max(rv$proxy$DOL)) {
      disable("all_length")
    } else {
      enable("all_length")
    }
  })
  
  observeEvent(input$HC_range, {
    click("calculate")
    change_color_theme()
    range <- input$HC_range
    if (range[1] == 0 & range[2] == max(rv$proxy$DOL)) {
      disable("all_HC")
    } else {
      enable("all_HC")
    }
  })
  
  observeEvent(input$BW_regained, {
    reset_weight_assessment()
    BW <- rv$proxy$Weight[1]
    updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c((max(rv$proxy$DOL[!is.na(rv$proxy$Weight) & as.numeric(rv$proxy$Weight) < BW])+1), max(rv$proxy$DOL)))
  })
  
  observeEvent(input$last_seven, {
    reset_weight_assessment()
    updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c((max(rv$proxy$DOL[!(is.na(rv$proxy$Weight) | rv$proxy$Weight == "")])-6), max(rv$proxy$DOL[!is.na(rv$proxy$Weight)])))
  })
  
  observeEvent(input$first_twentyeight, {
    reset_weight_assessment()
    updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c(0, 27))
  })
  
  observeEvent(input$seven_prior, {
    reset_weight_assessment()
    updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c(0,(max(rv$proxy$DOL[!(is.na(rv$proxy$Weight) | rv$proxy$Weight == "")])-7)))
  })
  
  observeEvent(input$all, {
    reset_weight_assessment()
    updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c(0, max(rv$proxy$DOL)))
  })
  
  observeEvent(input$all_length, {
    updateSliderInput(session, "length_range", min = 0, max = max(rv$proxy$DOL), value = c(0, max(rv$proxy$DOL)))
  })
  
  observeEvent(input$all_HC, {
    updateSliderInput(session, "HC_range", min = 0, max = max(rv$proxy$DOL), value = c(0, max(rv$proxy$DOL)))
  })
  
  observeEvent(input$weight_assess, {
    
    reset_weight_assessment()
    
    minDOL <- min(rv$dt$DOL[!is.na(rv$dt$Weight)])
    maxDOL <- max(rv$dt$DOL[!is.na(rv$dt$Weight)])
    canAssess <- ifelse(maxDOL-minDOL > 6, TRUE, FALSE)
    
    if (canAssess) {
      updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c(minDOL, maxDOL))
      type <<- "all_tp"
      rv$assessing_weight <- 1
      
    } else {
      showModal(
        modalDialog(
          title = "",
          span(style=paste0("color:",colors$color9,";"), "This infant is not 1 week old yet.")
        )
      )
      
    }
    
  })
  
  observeEvent(rv$all_tp, {
    if (!is.na(rv$all_tp)) {
      maxDOL <- max(rv$dt$DOL[!is.na(rv$dt$Weight)])
      
      last_seven_table <- rv$dt %>% filter(DOL %in% c((maxDOL-6):maxDOL), !is.na(Weight)) %>%
        mutate(Weight_Z = as.numeric(str_extract(Weight_Z, "(?<=\\()[0-9]+\\.[0-9]+")))
      
      rv$lastThree <- last_seven_table$Weight_Z[c(pmax((nrow(last_seven_table)-2),0):nrow(last_seven_table))]
      rv$lastThree <- rv$lastThree[!is.na(rv$lastThree)]
      updateSliderInput(session, "weight_range", min = 0, max = max(rv$proxy$DOL), value = c((max(rv$proxy$DOL[!(is.na(rv$proxy$Weight) | rv$proxy$Weight == "")])-6), max(rv$proxy$DOL[!is.na(rv$proxy$Weight)])))
      type <<- "last_seven_tp"
      
    }
  })
  
  weight_assess <- reactive(
    list(rv$lastThree, rv$last_seven_tp, rv$all_tp)
  )
  
  observeEvent(weight_assess(), {
    
    if (all(!is.na(rv$lastThree), !is.na(rv$all_tp), !is.na(rv$last_seven_tp), rv$assessing_weight == 1)) {
      test1 <- ifelse(all(rv$lastThree < rv$all_tp), "ALL", "NOT all")
      test2 <- ifelse((rv$all_tp - rv$last_seven_tp) > floor(rv$all_tp/10), "FAIL", "PASS")
      
      if (test1 == "ALL" | test2 == "FAIL") {
        recommendation <- "Consider increasing calories"
      } else {
        recommendation <- "Consider keeping the same calories"
      }
      
      rv$assessing_weight <- 0
      showModal(
        modalDialog(
          title = span("Weight Assessment", style=paste0("font-family:", rv$font,";color:", colors$color2, ";")),
          footer = actionButton("done_btn_weight_assess", span("DONE", style=paste0("border:0;font-family:", rv$font,";")), style=paste0("border:0;color:", colors$color3, ";background-color:", colors$color6, ";")),
          div(style=paste0("line-height:150%;font-family:",rv$font,";color:", colors$color2, ";"),
              paste0("Overall Trajectory: ", format(nsmall = 1, rv$all_tp), " percentile"),br(),
              paste0("Trajectory in last 7 days: ", format(nsmall = 1, rv$last_seven_tp), " percentile"), br(),
              HTML(paste0("Trajectory difference: <span style='color:", colors$color1, ";'>", format(nsmall = 1, rv$last_seven_tp - rv$all_tp), "</span>")), br(),
              HTML(paste0("Last three measurements <span style='color:", colors$color1, ";'>", test1, "</span> below the overall trajectory")), br(),
              hr(),
              HTML(paste0("Recommendation: ", "<span style='color:", colors$color9, ";'>", recommendation, "</span>")))
        )
      )
      click("all")
    }
  })
  
  observeEvent(input$done_btn_weight_assess, {
    removeModal()
  })
  
  
  observeEvent(rv$dt, {
    if (nrow(rv$dt)>0) {
      
      if (input$edit_mode) {
        enable("save_data")
        enable('delete_record')
      } else {
        disable("save_data")
        disable("delete_record")
      }
      
      enable("calculate")
      enable("discharged")
      enable("weight_assess")
      disable("GA_week")
      disable("GA_day")
      disable("sex")
    } else {
      disable("save_data")
      disable("calculate")
      disable("delete_record")
      disable("discharged")
      disable("weight_assess")
    }
  })
  
  observeEvent(input$meas_table_cell_edit, {
    info <- input$meas_table_cell_edit
    
    if (info$col == 3) {
      if (is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "Weight"] <- NA
        #rv$dt[info$row, "Weight"] <- NA
      } else if (!is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "Weight"] <- as.numeric(info$value)
        #rv$dt[info$row, "Weight"] <- as.numeric(info$value)
      }
    } else if (info$col == 5) {
      if (is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "Length"] <- NA
        #rv$dt[info$row, "Length"] <- NA
      } else if (!is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "Length"] <- as.numeric(info$value)
        #rv$dt[info$row, "Length"] <- as.numeric(info$value)
      }
    } else if (info$col == 7) {
      if (is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "HC"] <- NA
        #rv$dt[info$row, "HC"] <- NA
      } else if (!is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "HC"] <- as.numeric(info$value)
        #rv$dt[info$row, "HC"] <- as.numeric(info$value)
      }
    }
  })
  
  observeEvent(input$calculate, {
    if (nrow(rv$proxy)> 0) {
      

      for (i in c(1:nrow(rv$proxy))) {
        if (!is.na(rv$proxy[i, "Weight"])) {
          CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[i, "DOL"]) - rv$offset
          expected_weight <- rv$Chou_weight[rv$Chou_weight$Day %in% CGA_adjust, "percentile_50_var"]
          expected_sigma  <- rv$Chou_weight[rv$Chou_weight$Day %in% CGA_adjust, "sigma"]
          
          Z <- (as.numeric(rv$proxy[i, "Weight"]) / 1000 - expected_weight) / expected_sigma
          Percentile <- 100 * pnorm(Z)
          rv$proxy[i, "Weight_Z"] <- paste0(format(nsmall = 2, round(digits = 2, Z)), " (", format(round(digits = 1, Percentile), nsmall = 1), "%ile)")
        } else if (is.na(rv$proxy[i, "Weight"])) {
          rv$proxy[i, "Weight_Z"] <- NA
        }
        
        if (!is.na(rv$proxy[i, "Length"])) {
          CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[i, "DOL"]) - rv$offset
          expected_length <- rv$Chou_length[rv$Chou_length$Day %in% CGA_adjust, "percentile_50_var"]
          expected_sigma  <- rv$Chou_length[rv$Chou_length$Day %in% CGA_adjust, "sigma"]
          
          Z <- (as.numeric(rv$proxy[i, "Length"]) - expected_length) / expected_sigma
          Percentile <- 100 * pnorm(Z)
          rv$proxy[i, "Length_Z"] <- paste0(format(nsmall=2, round(digits = 2, Z)), " (", format(round(digits = 1, Percentile), nsmall = 1), "%ile)")
        } else if (is.na(rv$proxy[i, "Length"])) {
          rv$proxy[i, "Length_Z"] <- NA
        }
        
        if (!is.na(rv$proxy[i, "HC"])) {
          CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[i, "DOL"]) - rv$offset
          expected_HC <- rv$Chou_HC[rv$Chou_HC$Day %in% CGA_adjust, "percentile_50_var"]
          expected_sigma  <- rv$Chou_HC[rv$Chou_HC$Day %in% CGA_adjust, "sigma"]
          
          Z <- (as.numeric(rv$proxy[i, "HC"]) - expected_HC) / expected_sigma
          Percentile <- 100 * pnorm(Z)
          rv$proxy[i, "HC_Z"] <- paste0(format(nsmall=2, round(digits = 2, Z)), " (", format(round(digits = 1, Percentile), nsmall = 1), "%ile)")
        } else if (is.na(rv$proxy[i, "HC"])) {
          rv$proxy[i, "HC_Z"] <- NA
        }
        
      }
      
      
      
      ### WEIGHT
      
      percentile_df <- rv$proxy %>% 
        filter(DOL >= as.numeric(input$weight_range[1]), DOL <= as.numeric(input$weight_range[2])) %>% 
        left_join(rv$Chou_weight %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
      percentile <- seq(0.1, 99.9, by = 0.1)
      
      residual_df <- data.frame()
      
      if (!all(is.na(percentile_df$Weight))) {
        for (j in percentile) {
          percentile_df$percentile <- 1000 * (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
          percentile_df$residual <- percentile_df$Weight - percentile_df$percentile
          temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
          residual_df <- rbind(residual_df, temp)
          
          
        }
        rv$percentile_min_weight <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
        rv$Chou_weight$percentile <- rv$Chou_weight$Predicted_expected + qnorm(rv$percentile_min_weight/100) * rv$Chou_weight$sigma
        output$weight_trajectory_percentile <- renderUI({
          div(style=paste0("font-size:28px;font-weight:500;color:",colors$color9,";"), HTML(ifelse(rv$percentile_min_weight == 99.9, "&ge;", ifelse(rv$percentile_min_weight == 0.1, "&le;", "")), 
                                                                                            paste0(rv$percentile_min_weight,"% (", round(digits = 2, qnorm(rv$percentile_min_weight/100)), ")")))
        })
        
        weight_WHO_percentile <- rv$WHO_weight[rv$WHO_weight$Percentile %in% rv$percentile_min_weight, "MEAS"]
        if (max(rv$Chou_weight$percentile) < (weight_WHO_percentile/1000)) {
          Percentile_PMA <- NA
        } else if (max(rv$Chou_weight$percentile) >= (weight_WHO_percentile/1000)) {
          Percentile_PMA <- rv$Chou_weight %>% filter(percentile < (weight_WHO_percentile/1000)) %>% slice(n())
          Percentile_PMA <- Percentile_PMA$Day + rv$offset
        }
        
        weight_transition_WHO <- 
          ifelse(is.na(Percentile_PMA), 
                 paste0((44*7+rv$offset)%/%7," weeks ", (44*7+rv$offset)%%7, " days PMA<div style='font-size:12px;'>(Size for percentile not attainable)</div>"),
                 paste0(weight_WHO_percentile, " grams (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)"))
        
        output$weight_WHO_transition <- renderUI({
          div(style=paste0("font-size:20px;font-weight:300;color:",colors$color9,";"), HTML(weight_transition_WHO))
        })
        
        rv$Chou_weight <- rv$Chou_weight %>%
          arrange(Daily_DSB) %>%
          mutate(day_delta = Daily_DSB - lag(Daily_DSB),
                 percentile_rate = (percentile - lag(percentile)) / day_delta,
                 percentile_rate_kg = percentile_rate / percentile)
        
        if (length(rv$Chou_weight$percentile_rate_kg[rv$Chou_weight$percentile > rv$Chou_weight$percentile[1] & rv$Chou_weight$Day < 34*7])>0) {
          rate_acceleration <- paste0(round(digits = 1, mean(1000 * rv$Chou_weight$percentile_rate_kg[rv$Chou_weight$percentile > rv$Chou_weight$percentile[1] & rv$Chou_weight$Day < 34*7])),
                                      "±",
                                      round(digits = 1,   sd(1000 * rv$Chou_weight$percentile_rate_kg[rv$Chou_weight$percentile > rv$Chou_weight$percentile[1] & rv$Chou_weight$Day < 34*7])),
                                      " g/kg/day")
        } else {
          rate_acceleration <- "Not Applicable"
        }
        
        
        rate_stable       <- paste0(round(digits = 1, mean(1000 * rv$Chou_weight$percentile_rate[rv$Chou_weight$percentile > rv$Chou_weight$percentile[1] & rv$Chou_weight$Day >= 34*7])),
                                    "±",
                                    round(digits = 1, sd(1000 * rv$Chou_weight$percentile_rate[rv$Chou_weight$percentile > rv$Chou_weight$percentile[1] & rv$Chou_weight$Day >= 34*7])),
                                    " g/day")
        
        output$rate_acceleration <- renderUI({
          div(style=paste0("font-size:18px;font-weight:300;color:",colors$color9,";"), HTML(rate_acceleration))
        })
        
        output$rate_stable <- renderUI({
          div(style=paste0("font-size:18px;font-weight:300;color:",colors$color9,";"), HTML(rate_stable))
        })
        
        if (!is.na(type)) {
          rv[[type]] <- rv$percentile_min_weight
        }
        
        
      } else {
        rv$percentile_min_weight <- NA
        rv$Chou_weight$percentile <- as.numeric(NA)
        output$weight_trajectory_percentile <- renderUI({})
        output$weight_WHO_transition <- renderUI({})
        output$rate_acceleration <- renderUI({})
        output$rate_stable <- renderUI({})
      }
      
      
      ### LENGTH
      
      percentile_df <- rv$proxy %>% 
        filter(DOL >= as.numeric(input$length_range[1]), DOL <= as.numeric(input$length_range[2])) %>% 
        left_join(rv$Chou_length %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
      residual_df <- data.frame()
      
      if (!all(is.na(percentile_df$Length))) {
        
        for (j in percentile) {
          percentile_df$percentile <- (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
          percentile_df$residual <- percentile_df$Length - percentile_df$percentile
          temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
          residual_df <- rbind(residual_df, temp)
          
        }
        rv$percentile_min_length <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
        rv$Chou_length$percentile <- rv$Chou_length$Predicted_expected + qnorm(rv$percentile_min_length/100) * rv$Chou_length$sigma
        output$length_trajectory_percentile <- renderUI({
          div(style=paste0("font-size:28px;font-weight:500;color:",colors$color9,";"), HTML(ifelse(rv$percentile_min_length == 99.9, "&ge;", ifelse(rv$percentile_min_length == 0.1, "&le;", "")), 
                                                                                            paste0(rv$percentile_min_length,"% (", round(digits = 2, qnorm(rv$percentile_min_length/100)), ")")))
        })
        
        length_WHO_percentile <- rv$WHO_length[rv$WHO_length$Percentile %in% rv$percentile_min_length, "MEAS"]
        if (max(rv$Chou_length$percentile) < (length_WHO_percentile)) {
          Percentile_PMA <- NA
        } else if (max(rv$Chou_length$percentile) >= (length_WHO_percentile)) {
          Percentile_PMA <- rv$Chou_length %>% filter(percentile < (length_WHO_percentile)) %>% slice(n())
          Percentile_PMA <- Percentile_PMA$Day + rv$offset
        }
        
        length_transition_WHO <- 
          ifelse(is.na(Percentile_PMA), 
                 paste0((44*7+rv$offset)%/%7," weeks ", (44*7+rv$offset)%%7, " days PMA<div style='font-size:12px;'>(Size for percentile not attainable)</div>"),
                 paste0(length_WHO_percentile, " cm (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)"))
        
        output$length_WHO_transition <- renderUI({
          div(style=paste0("font-size:20px;font-weight:300;color:",colors$color9,";"), HTML(length_transition_WHO))
        })
        
      } else {
        rv$percentile_min_length <- NA
        rv$Chou_length$percentile <- as.numeric(NA)
        output$length_trajectory_percentile <- renderUI({})
        output$length_WHO_transition <- renderUI({})
      }
      
      ### HEAD CIRCUMFERENCE
      
      percentile_df <- rv$proxy  %>% 
        filter(DOL >= as.numeric(input$HC_range[1]), DOL <= as.numeric(input$HC_range[2])) %>% 
        left_join(rv$Chou_HC %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
      residual_df <- data.frame()
      
      
      if (!all(is.na(percentile_df$HC))) {
        
        for (j in percentile) {
          percentile_df$percentile <- (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
          percentile_df$residual <- percentile_df$HC - percentile_df$percentile
          temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
          residual_df <- rbind(residual_df, temp)
      
        }
        rv$percentile_min_HC <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
        rv$Chou_HC$percentile <- rv$Chou_HC$Predicted_expected + qnorm(rv$percentile_min_HC/100) * rv$Chou_HC$sigma
        output$HC_trajectory_percentile <- renderUI({
          div(style=paste0("font-size:28px;font-weight:500;color:",colors$color9,";"), HTML(ifelse(rv$percentile_min_HC == 99.9, "&ge;", ifelse(rv$percentile_min_HC == 0.1, "&le;", "")), 
                                                                                            paste0(rv$percentile_min_HC,"% (", round(digits = 2, qnorm(rv$percentile_min_HC/100)), ")")))
        })
        
        HC_WHO_percentile <- rv$WHO_HC[rv$WHO_HC$Percentile %in% rv$percentile_min_HC, "MEAS"]
        if (max(rv$Chou_HC$percentile) < (HC_WHO_percentile)) {
          Percentile_PMA <- NA
        } else if (max(rv$Chou_HC$percentile) >= (HC_WHO_percentile)) {
          Percentile_PMA <- rv$Chou_HC %>% filter(percentile < (HC_WHO_percentile)) %>% slice(n())
          Percentile_PMA <- Percentile_PMA$Day + rv$offset
        }
        
        HC_transition_WHO <- 
          ifelse(is.na(Percentile_PMA), 
                 paste0((44*7+rv$offset)%/%7," weeks ", (44*7+rv$offset)%%7, " days PMA<div style='font-size:12px;'>(Size for percentile not attainable)</div>"),
                 paste0(HC_WHO_percentile, " cm (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)"))
        
        output$HC_WHO_transition <- renderUI({
          div(style=paste0("font-size:20px;font-weight:300;color:",colors$color9,";"), HTML(HC_transition_WHO))
        })
        
      } else {
        rv$percentile_min_length <- NA
        rv$Chou_HC$percentile <- as.numeric(NA)
        output$HC_trajectory_percentile <- renderUI({})
        output$HC_WHO_transition <- renderUI({})
      }
      
      
      rv$dt <- rv$proxy

      
    }
  })
  
  
  observeEvent(input$color_theme_selection, {
    selected_color_theme <- gsub("Option ", "", str_extract(input$color_theme_selection, "Option\\s[0-9]+"))
    update_color_input(selected_color_theme)
    
  })

  
  observeEvent(list(input$plot_output_size, input$plot_output_orientation), {
    if (req(input$plot_output_orientation) == "Portrait") {
      if (req(input$plot_output_size) == "Letter") {
        rv$plot_width_x <- 7.5
        rv$plot_height_y <- 11
      } else if (req(input$plot_output_size) == "Legal") {
        rv$plot_width_x <- 10
        rv$plot_height_y <- 17
      } else if (req(input$plot_output_size) == "A4") {
        rv$plot_width_x <- 7.5
        rv$plot_height_y <- 11.7
      } else if (req(input$plot_output_size) == "B5") {
        rv$plot_width_x <- 6
        rv$plot_height_y <- 9.8
      }
    } else if (req(input$plot_output_orientation) == "Landscape") {
      if (req(input$plot_output_size) == "Letter") {
        rv$plot_height_y <- 7.5
        rv$plot_width_x <- 11
      } else if (req(input$plot_output_size) == "Legal") {
        rv$plot_height_y <- 10
        rv$plot_width_x <- 17
      } else if (req(input$plot_output_size) == "A4") {
        rv$plot_height_y <- 7.5
        rv$plot_width_x <- 11.7
      } else if (req(input$plot_output_size) == "B5") {
        rv$plot_height_y <- 6
        rv$plot_width_x <- 9.8
      }
    }
    
    output$msg <- renderUI({
      span(rv$plot_width_x,"|",rv$plot_height_y, style="color:white;")
    })
    
    output$download_plot_msg <- renderUI({
      div(rv$layout, " layout, size of ", rv$plot_width_x, "inches by ", rv$plot_height_y, "inches", style="color:red;padding:10px 0 20px 0;")
    })
  })
  
  observeEvent(input$plot_layout_1, {
    rv$layout <- "Horizontal"
    plots$layout <- plots$weight_plot / plots$length_plot / plots$HC_plot
    output$download_plot_msg <- renderUI({
      div(rv$layout, " layout, size of ", rv$plot_width_x, "inches by ", rv$plot_height_y, "inches", style="color:red;padding:10px 0 20px 0;")
    })
  })
  
  observeEvent(input$plot_layout_2, {
    rv$layout <- "Vertical"
    plots$layout <- plots$weight_plot | plots$length_plot | plots$HC_plot
    output$download_plot_msg <- renderUI({
      div(rv$layout, " layout, size of ", rv$plot_width_x, "inches by ", rv$plot_height_y, "inches", style="color:red;padding:10px 0 20px 0;")
    })
  })
  
  observeEvent(input$plot_layout_3, {
    rv$layout <- "Plate"
    plots$layout <- plots$weight_plot | (plots$length_plot / plots$HC_plot)
    output$download_plot_msg <- renderUI({
      div(rv$layout, " layout, size of ", rv$plot_width_x, "inches by ", rv$plot_height_y, "inches", style="color:red;padding:10px 0 20px 0;")
    })
  })
  
  
  output$download_plot <- downloadHandler(
    
    filename = function() {
      paste0(input$id_dropdown, "_plot.pdf")
    },
    content = function(file) {
      
      if (rv$hasDisplay == 1) {
        pdf(file, width = rv$plot_width_x, height = rv$plot_height_y)
        if (rv$layout == "Vertical") {
          print(((plots$weight_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) / (plots$length_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) / (plots$HC_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch")))) +
                  plot_annotation(subtitle = "nicugrowth.app",
                                  title = "Postnatal Growth Charts for Perterm Infants",
                                  caption = "Contact us at admin@nicugrowth.app for any questions."))
        } else if (rv$layout == "Horizontal") {
          print(((plots$weight_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) | (plots$length_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) | (plots$HC_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch")))) +
                  plot_annotation(subtitle = "nicugrowth.app",
                                  title = "Postnatal Growth Charts for Perterm Infants",
                                  caption = "Contact us at admin@nicugrowth.app for any questions."))
        } else if (rv$layout == "Plate") {
          print(((plots$weight_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) | ((plots$length_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))) / (plots$HC_plot & theme(plot.margin = margin(0.1,0.1,0.1,0.1,"inch"))))) +
                  plot_annotation(subtitle = "nicugrowth.app",
                                  title = "Postnatal Growth Charts for Perterm Infants",
                                  caption = "Contact us at admin@nicugrowth.app for any questions."))
        }
        dev.off()
      }
    }
  )
  
  observeEvent(input$table_output_format, {
    if (input$table_output_format == "CSV") {
      rv$output_table_format <- "csv"
    } else if (input$table_output_format == "MS Excel") {
      rv$output_table_format <- "xlsx"
    } else if (input$table_output_format == "MS Word") {
      rv$output_table_format <- "docx"
    }
    
    output$msg <- renderUI({
      span(rv$plot_width_x,"|",rv$plot_height_y, style="color:white;")
    })
    output$download_table_msg <- renderUI({
      div("The table will be saved in ", input$table_output_format, " format", style="color:red;padding:10px 0 20px 0;")
    })
  })
  
  table_output <- reactive({
    proxy_df <- rv$proxy
    proxy_df[is.na(proxy_df)] <- ""
    proxy_df  %>% 
      rename(`Weight Z (%ile)` ="Weight_Z",
             `Length Z (%ile)` = "Length_Z",
             `HC Z (%ile)` = "HC_Z") %>% 
      select(input$table_output_columns)
    
  })
  
  output$download_table <- downloadHandler(
    
    filename = function() {
      paste0(input$id_dropdown, "_table.", rv$output_table_format)
    },
    content = function(file) {
      
      if (rv$output_table_format == "csv") {
        write_csv(table_output(), file = file, na = "")
      } else if (rv$output_table_format == "xlsx") {
        writexl::write_xlsx(table_output(), file)
      } else if (rv$output_table_format == "docx") {
        officer::read_docx() %>%
          officer::body_add_table(., table_output()) %>%
          print(., file)
      }
      
    }
  )
  
  observeEvent(input$backtotop, {
    runjs('document.getElementById("main_frame").scrollIntoView({block:"start", behavior:"smooth"})')  
    
  })
  
  
  
})
