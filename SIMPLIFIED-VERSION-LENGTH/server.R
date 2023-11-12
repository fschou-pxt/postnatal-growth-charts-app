library(shiny)
library(duckdb)
library(DBI)
library(tidyverse)
library(DT)
library(shinyjs)
library(lubridate)

shinyServer(function(input, output) {

    rv <- reactiveValues(Chou_length = data.frame(), 
                         length_dt = data.frame(DOL = numeric(0), PMA = numeric(0), Length = numeric(0), Z = numeric(0), Percentile = numeric(0)),
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
        rv$length_dt <- data.frame(DOL = numeric(0), CGA = numeric(0), Length = numeric(0), Z = numeric(0), Percentile = numeric(0))
        
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
          output$length_plot <- renderPlot({})
          } else if (GAn>22 & GAn<35) {
          
            if (input$sex == "") {
                output$msg <- renderUI({
                    div(style="color:#cc0000;font-weight:500;font-size:16px;padding-left:10px;", "Please enter infant sex!")
                })
            } else if (input$sex %in% c("Male", "Female")) {
                ### retrieve data
                con <- dbConnect(duckdb::duckdb(), dbdir="../DATABASE/db.duckdb", read_only=TRUE)
                rv$Chou_length <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn,"_",req(input$sex),"_Length")) %>% mutate(percentile = as.numeric(NA))
                dbDisconnect(con, shutdown = TRUE)
                
                rv$length_dt <-
                    data.frame(
                        DOL = c(0:(44*7-GAdays)),
                        PMA = c(GAdays:(44*7)),
                        Length = NA,
                        Z = NA
                    ) %>%
                    mutate(Week = PMA %/% 7, Day = PMA %% 7)
                
                rv$proxy <- rv$length_dt
                
                output$table <- renderUI({
                    fluidRow(
                        div(DTOutput("lg_table"), style = "height:380px;"),
                        div(style="padding:6px 0 0 0;", 
                            column(2, offset = 10, align="right", style="padding-right:0px;", disabled(actionButton("calculate", "PLOT", style="padding:3px 6px 3px 6px;font-size:14px;background-color:#546E7A;color:#FFFFFF;border:0;")))
                        )
                    )
                })
                
                output$lg_table <- renderDT({
                    datatable(rv$length_dt %>% select(DOL, Week, Day, Length, Z) %>% rename(`Z(%ile)` ="Z"), 
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
                
                
                output$length_plot <- renderPlot({

                    ggplot(rv$Chou_length, aes(x = ((Day + rv$offset)/7))) +
                        geom_line(aes(y = percentile_3_var), size = 0.75, color = "#546E7A", linetype = "solid") +
                        geom_line(aes(y = percentile_10_var), size = 0.5, color = "#546E7A", linetype = "dashed") +
                        geom_line(aes(y = percentile_25_var), size = 0.5, color = "#546E7A", linetype = "dotted") +
                        geom_line(aes(y = percentile_50_var), size = 0.5, color = "#546E7A", linetype = "solid") +
                        geom_line(aes(y = percentile_75_var), size = 0.5, color = "#546E7A", linetype = "dotted") +
                        geom_line(aes(y = percentile_90_var), size = 0.5, color = "#546E7A", linetype = "dashed") +
                        geom_line(aes(y = percentile_97_var), size = 0.75, color = "#546E7A", linetype = "solid") +
                        geom_point(data = rv$length_dt %>% filter(!is.na(Length)), aes(x = PMA/7, y = Length), color = "#CC0000", size = 3) +
                        geom_line(data = rv$Chou_length, 
                                  mapping = aes(x = ((Day + rv$offset)/7), y = percentile), size = 0.75, color = "#F1C231") +
                        labs(x = "Postmenstrual Age (week)", 
                             y = "Length (cm)",
                             title = NULL) + 
                        scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GAn + rv$offset/7)), breaks =c(0:21), name = "Week of Life")) +
                        scale_y_continuous(breaks = 2 * c(0:50)) +
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
    
    observeEvent(input$lg_table_cell_edit, {
        info <- input$lg_table_cell_edit
        if (info$col == 3) {
            
            if (is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "Length"] <- NA
            } else if (!is.na(as.numeric(info$value))) {
                rv$proxy[info$row, "Length"] <- as.numeric(info$value)
                CGA_adjust <- as.numeric(input$GA_week) * 7 + as.numeric(input$GA_day) + as.numeric(rv$proxy[info$row, "DOL"]) - rv$offset
                expected_length <- rv$Chou_length[rv$Chou_length$Day %in% CGA_adjust, "percentile_50_var"]
                expected_sigma  <- rv$Chou_length[rv$Chou_length$Day %in% CGA_adjust, "sigma"]

                Z <- (as.numeric(info$value) - expected_length) / expected_sigma
                Percentile <- 100 * pnorm(Z)
                rv$proxy[info$row, "Z"] <- paste0(round(digits = 2, Z), " (", round(digits = 1, Percentile), ")")
                
                percentile_df <- rv$proxy %>% left_join(rv$Chou_length %>% select(Day, Predicted_expected, sigma) %>% mutate(PMA = Day + rv$offset) %>% select(-Day))
                percentile <- seq(0.1, 99.9, by = 0.1)
                
                residual_df <- data.frame()
                for (j in percentile) {
                    percentile_df$percentile <- (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
                    percentile_df$residual <- percentile_df$Length - percentile_df$percentile
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
        rv$length_dt <- rv$proxy
        
        output$msg <- renderUI({
            div(paste0("Trajectory percentile: ", rv$percentile_min, "%ile"), style="color:#cc0000;padding-top:3px;font-weight:500;font-size:14px;")
        })
        rv$Chou_length$percentile <- rv$Chou_length$Predicted_expected + qnorm(rv$percentile_min/100) * rv$Chou_length$sigma
        disable("calculate")
    })
    

})
