library(shiny)
library(duckdb)
library(DBI)
library(tidyverse)
library(DT)
library(shinyjs)
library(lubridate)

shinyServer(function(session, input, output) {
  
  rv <- reactiveValues(Chou_weight = data.frame(), 
                       weight_dt = data.frame(DOL = numeric(0), PMA = numeric(0), Weight = numeric(0), Z = numeric(0), Percentile = numeric(0)),
                       offset = NA,
                       percentile_min = NA
  )
  
  basic_settings <- reactive(
    list(input$GA_week, input$GA_day, input$sex)
  )
  
  observeEvent(basic_settings(), {
    enable("plot")
  })
  
  observeEvent(input$plot, {
    disable("plot")
    
    GAdays <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) #calculate GA in days
    GAn <- round(GAdays/7)
    rv$offset <- GAdays - (GAn * 7)
    rv$weight_dt <- data.frame(DOL = numeric(0), CGA = numeric(0), Weight = numeric(0), Z = numeric(0), Percentile = numeric(0))
    
    output$msg <- renderUI({})
    
    if (is.na(GAn)) {
      output$msg <- renderUI({
        div(style="color:#cc0000;font-weight:500;font-size:16px;padding-left:10px;", "Please enter GA!")
      })
    } else if (GAn<=22 | GAn>=35) {
      output$msg <- renderUI({
        div(style="color:#cc0000;font-weight:500;font-size:16px;padding-left:10px;", "GA Out of Range!")
      })
      output$table <- renderUI({})
      output$weight_plot <- renderPlot({})
    } else if (GAn>22 & GAn<35) {
      
      if (input$sex == "") {
        output$msg <- renderUI({
          div(style="color:#cc0000;font-weight:500;font-size:16px;padding-left:10px;", "Please enter infant sex!")
        })
      } else if (input$sex %in% c("Male", "Female")) {
        ### retrieve data
        con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
        rv$Chou_weight <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_Weight")) %>% mutate(percentile = as.numeric(NA))
        dbDisconnect(con, shutdown = TRUE)
        
        rv$weight_dt <-
          data.frame(
            DOL = c(0:(44*7-GAdays)),
            PMA = c(GAdays:(44*7)),
            Weight = NA,
            Z = NA
          ) %>%
          mutate(Week = PMA %/% 7, Day = PMA %% 7)
        
        rv$proxy <- rv$weight_dt
        
        output$table <- renderUI({
          fluidRow(
            div(DTOutput("wt_table"), style = "height:380px;"),
            div(style="padding:6px 0 0 0;", 
                column(2, offset=10, align="right", style="padding-right:0px;", disabled(actionButton("calculate", "PLOT", style="padding:3px 6px 3px 6px;font-size:14px;background-color:#546E7A;color:#FFFFFF;border:0;")))
            )
          )
        })
        
        output$wt_table <- renderDT({
          datatable(rv$weight_dt %>% select(DOL, Week, Day, Weight, Z) %>% rename(`Z(%ile)` ="Z"), 
                    rownames = FALSE, 
                    selection = "none", 
                    editable = list(target = "cell", disable = list(columns = c(0,1,2,4))),
                    options = list(dom = "t", 
                                   paging = FALSE,
                                   scrollY = "340",
                                   scrollX = TRUE,
                                   columnDefs = list(list(className = 'dt-right', targets = "_all"),
                                                     list(width = "17.5%", targets = c(0,1,2,3)))))
        })
        
        
        output$weight_plot <- renderPlot({
          
          ggplot(rv$Chou_weight, aes(x = ((Day + rv$offset)/7))) +
            geom_line(aes(y = percentile_3_var * 1000), size = 0.75, color = "#546E7A", linetype = "solid") +
            geom_line(aes(y = percentile_10_var * 1000), size = 0.5, color = "#546E7A", linetype = "dashed") +
            geom_line(aes(y = percentile_25_var * 1000), size = 0.5, color = "#546E7A", linetype = "dotted") +
            geom_line(aes(y = percentile_50_var * 1000), size = 0.5, color = "#546E7A", linetype = "solid") +
            geom_line(aes(y = percentile_75_var * 1000), size = 0.5, color = "#546E7A", linetype = "dotted") +
            geom_line(aes(y = percentile_90_var * 1000), size = 0.5, color = "#546E7A", linetype = "dashed") +
            geom_line(aes(y = percentile_97_var * 1000), size = 0.75, color = "#546E7A", linetype = "solid") +
            geom_point(data = rv$weight_dt %>% filter(!is.na(Weight)), aes(x = PMA/7, y = Weight), color = "#CC0000", size = 3) +
            geom_line(data = rv$Chou_weight, 
                      mapping = aes(x = ((Day + rv$offset)/7), y = percentile * 1000), size = 0.75, color = "#F1C231") +
            labs(x = "Postmenstrual Age (week)", 
                 y = "Weight (gram)",
                 title = NULL) + 
            scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
            scale_y_continuous(breaks = 500 * c(0:20)) +
            theme_bw() +
            theme(axis.text = element_text(size = 12, color = "black"),
                  axis.title.x = element_text(size = 14, hjust = 1, face = "bold"),
                  axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 0.1, 0, 0, "inch")),
                  plot.margin = margin(0, 0, 0, 0, "cm"),
                  plot.subtitle = element_text(size = 14, hjust = 0.5, colour = "#a57164", face = "bold"))         
          
        })
      }
      
    } 
  })
  
  observeEvent(input$wt_table_cell_edit, {
    info <- input$wt_table_cell_edit
    if (info$col == 3) {
      
      if (is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "Weight"] <- NA
      } else if (!is.na(as.numeric(info$value))) {
        rv$proxy[info$row, "Weight"] <- as.numeric(info$value)
        CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[info$row, "DOL"]) - rv$offset
        expected_weight <- rv$Chou_weight[rv$Chou_weight$Day %in% CGA_adjust, "percentile_50_var"]
        expected_sigma  <- rv$Chou_weight[rv$Chou_weight$Day %in% CGA_adjust, "sigma"]
        
        Z <- (as.numeric(info$value) / 1000 - expected_weight) / expected_sigma
        Percentile <- 100 * pnorm(Z)
        rv$proxy[info$row, "Z"] <- paste0(round(digits = 2, Z), " (", round(digits = 1, Percentile), ")")
        
        percentile_df <- rv$proxy %>% left_join(rv$Chou_weight %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
        percentile <- seq(0.1, 99.9, by = 0.1)
        
        residual_df <- data.frame()
        for (j in percentile) {
          percentile_df$percentile <- 1000 * (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
          percentile_df$residual <- percentile_df$Weight - percentile_df$percentile
          temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
          residual_df <- rbind(residual_df, temp)
        }
        rv$percentile_min <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
        output$msg <- renderUI({})
        enable("calculate")
      }
      
    }
    
    
  })
  
  observeEvent(input$calculate, {
    rv$weight_dt <- rv$proxy
    
    output$msg <- renderUI({
      div(paste0("Trajectory percentile: ", rv$percentile_min, "%ile"), style="color:#cc0000;padding-top:3px;font-weight:500;font-size:14px;")
    })
    updateTextInput(session, "enterTP", value = rv$percentile_min)
    #rv$Chou_weight$percentile <- rv$Chou_weight$Predicted_expected + qnorm(rv$percentile_min/100) * rv$Chou_weight$sigma
    disable("calculate")
    load("weight_log.RData")
    weight_log[[as.character(lubridate::now())]] <- rv$proxy
    save(weight_log, file = "weight_log.RData")
  })
  
  observeEvent(input$enterTP, {
    TP <- ifelse(!is.na(as.numeric(input$enterTP)), round(digits = 1, as.numeric(input$enterTP)), NA)
    
    if (is.na(TP) | TP > 99.9 | TP < 0.1) {
      updateTextInput(session, "enterTP", value = "")
      output$rate <- renderUI({})
    } else {
      GA_GROUP <- round(digits = 0, (as.numeric(req(input$GA_week)) * 7 + as.numeric(req(input$GA_day)))/7)
      TP_table <- readxl::read_excel("percentile_table_expanded.xlsx", sheet = paste0("GA", GA_GROUP, "_", req(input$sex), "_Weight"))
      phase2_mean <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_2_mean"])
      phase2_sd   <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_2_sd"])
      phase3_mean <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_3_mean"])
      phase3_sd   <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_3_sd"])
      phase2 <- paste0(format(nsmall = 1, phase2_mean), "±", format(nsmall = 1, phase2_sd))
      phase3 <- paste0(format(nsmall = 1, phase3_mean), "±", format(nsmall = 1, phase3_sd))      
      
      output$rate <- renderUI({
        div(style="margin-top:20px",
            tags$ul(
              tags$li(span("Weight acceleration phase: "), span(phase2,"g/kg/day")),
              tags$li(span("Stable weight gain phase: "), span(phase3,"g/day"))
            ))
      })
      
      rv$Chou_weight$percentile <- rv$Chou_weight$Predicted_expected + qnorm(TP/100) * rv$Chou_weight$sigma
      #enable("calculate")
    }
  })
  
})
