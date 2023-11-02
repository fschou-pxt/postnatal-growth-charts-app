library(plumber)

#* @apiTitle Postnatal Growth Charts for Preterm Infants

#* Health Check - Is the API running
#* @get /health-check

status <- function () {
  list(
    status = "All Good",
    time = Sys.time()
  )
}


growthCharts <- function(GA, SEX, TYPE) {
  library(tidyverse)
  
  colnames <- c('Daily_DSB', 'sex', 'Predicted_expected', 'Predicted_se_upper', 
                'Predicted_se_lower', 'intercept_var', 'Daily_DSB_var', 'covar', 
                'residual_var', 'sigma', 'percentile_3_var', 'percentile_10_var', 'percentile_25_var', 
                'percentile_50_var', 'percentile_75_var', 'percentile_90_var', 'percentile_97_var',
                'Day', 'VALUE', 'TP')
  chart <- as.data.frame(matrix(ncol=length(colnames), nrow = 0))
  colnames(chart) <- colnames
  
  if (as.numeric(GA) >=23 & as.numeric(GA) <=34 & SEX %in% c("Female", "Male")) {
    growthChartsTable <- readr::read_csv(paste0("Datatable/GA_", GA, "_", SEX, "_", TYPE, ".csv")) %>%
      dplyr::mutate(Day = Daily_DSB + GA * 7) %>% as.data.frame(.)
  }

}


df <- function(GA, SEX, TYPE, DOL, VALUE) {
  library(tidyverse)
  
  GA <- as.numeric(GA)
  df <- data.frame(Daily_DSB = as.numeric(DOL), VALUE = as.numeric(VALUE))
  growthCharts <- growthCharts(GA, SEX, TYPE)
  df_expanded <- df %>% left_join(growthCharts %>% select(Daily_DSB, Predicted_expected, sigma)) %>%
    rename(mean = "Predicted_expected",
           sd = "sigma") %>%
    mutate(mean = mean * ifelse(TYPE %in% "Weight", 1000, 1),
           sd = sd * ifelse(TYPE %in% "Weight", 1000, 1),
           `z-score` = (VALUE - mean)/sd,
           percentile = round(digits = 1, pnorm(`z-score`) * 100),
           `z-score`= round(digits = 2, `z-score`)) %>%
    select(-mean, -sd)
  return(df_expanded)
}

#* Assemble weight datatable and calculate z/percentile
#* @param DOL
#* @param VALUE
#* @param GA
#* @param SEX
#* @param FROM
#* @param TO
#* @post /datatable
#* @serializer csv
function(GA, SEX, TYPE, DOL = integer(0), VALUE = integer(0), FROM = as.integer(0), TO = integer(0)) {
  library(tidyverse)
  GA <- as.numeric(GA)
  TO <- ifelse(length(TO) == 0, (44-GA)*7, TO)
  
  df <- df(GA, SEX, TYPE, DOL, VALUE)
  df <- df[df$Daily_DSB %in% c(FROM:TO),]
  
  df <- df %>%
    rename(DOL = Daily_DSB) %>%
    as.data.frame(.)
  colnames(df)[colnames(df) %in% "VALUE"] <- TYPE
  return(df)
}


#* Retrieve trajectory percentile
#* @param GA
#* @param SEX
#* @param DOL
#* @param VALUE
#* @param FROM
#* @param TO
#* @post /TP
TP <- function(GA, SEX, TYPE, DOL = integer(0), VALUE = integer(0), FROM = 0, TO = integer(0)) {
  library(tidyverse)
  
  GA <- as.numeric(GA)
  growthChartsTable <- growthCharts(GA, SEX, TYPE)
  df <- data.frame()
  TO <- ifelse(length(TO) == 0, (44-GA)*7, TO)
  
  if (length(DOL) > 0 & length(VALUE) > 0 & length(DOL) == length(VALUE)) {
    df <- df(GA, SEX, TYPE, DOL, VALUE)
    df <- df[df$Daily_DSB %in% c(FROM:TO),]
  }
   
  if (nrow(df)> 0) {   
    percentile_df <- df %>% select(Daily_DSB, VALUE) %>% right_join(growthChartsTable %>% select(Daily_DSB, Day, Predicted_expected, sigma))
    percentile <- seq(0.1, 99.9, by = 0.1)
    
    residual_df <- data.frame()
    for (j in percentile) {
      percentile_df$percentile <- ifelse(TYPE %in% "Weight", 1000, 1) * (percentile_df$Predicted_expected + percentile_df$sigma * qnorm(j/100))
      percentile_df$residual <- percentile_df$VALUE - percentile_df$percentile
      temp <- data.frame(percentile = j, residual = sum(percentile_df$residual^2, na.rm = TRUE))
      residual_df <- rbind(residual_df, temp)
    }
    percentile_min <- residual_df$percentile[residual_df$residual == min(residual_df$residual, na.rm = TRUE)]
  } else {
    percentile_min <- NA
  }
  
  return(percentile_min)
}

#* Obtain growth rate during the weight acceleration phase
#* @param GA
#* @param SEX
#* @param DOL
#* @param VALUE
#* @param FROM
#* @param TO
#* @post /rate-acceleration-phase
function(GA, SEX, TYPE = "Weight", DOL = integer(0), VALUE = integer(0), FROM = 0, TO = numeric(0)) {
  TP <- TP(GA, SEX, TYPE, DOL, VALUE, FROM, TO)
  TP_table <- readxl::read_excel("Datatable/percentile_table_expanded.xlsx", sheet = paste0("GA", GA, "_", SEX, "_Weight"))
  phase2_mean <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_2_mean"])
  phase2_sd   <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_2_sd"])
  phase2 <- paste0(format(nsmall = 1, phase2_mean), "±", format(nsmall = 1, phase2_sd), " g/kg/day")
  return(phase2)
}

#* Obtain growth rate during the stable weight gain phase
#* @param GA
#* @param SEX
#* @param DOL
#* @param VALUE
#* @param FROM
#* @param TO
#* @post /rate-stable-phase
function(GA, SEX, TYPE = "Weight", DOL = integer(0), VALUE = integer(0), FROM = 0, TO = numeric(0)) {
  TP <- TP(GA, SEX, TYPE, DOL, VALUE, FROM, TO)
  TP_table <- readxl::read_excel("Datatable/percentile_table_expanded.xlsx", sheet = paste0("GA", GA, "_", SEX, "_Weight"))
  phase3_mean <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_3_mean"])
  phase3_sd   <- as.numeric(TP_table[TP_table$TP %in% as.character(TP), "rate_3_sd"])
  phase3 <- paste0(format(nsmall = 1, phase3_mean), "±", format(nsmall = 1, phase3_sd), " g/day")
  return(phase3)
}


#* Transitioning to WHO charts
#* @param GA
#* @param SEX
#* @param offset
#* @param DOL
#* @param VALUE
#* @param FROM
#* @param TO
#* @post /WHO-transitioning
function(GA, SEX, TYPE = "Weight", offset, DOL = integer(0), VALUE = integer(0), FROM = 0, TO = numeric(0)) {
  weight_transition_WHO <- NA
  weight_WHO_percentile <- 0
  GA <- as.numeric(GA)
  offset <- as.numeric(offset)
  
  library(tidyverse)
  
  growthChartsTable <- growthCharts(GA, SEX, TYPE)
  percentile_min <- TP(GA, SEX, TYPE, DOL, VALUE, FROM, TO)
  Percentile_PMA <- NA
  
  if (!is.na(percentile_min)) {
    growthChartsTable$TP <- (growthChartsTable$Predicted_expected + growthChartsTable$sigma * qnorm(percentile_min/100))
    
    WHOCharts <- readr::read_csv(paste0("Datatable/WHO_Weight_", SEX, ".csv")) %>% 
      mutate(Percentile = seq(0.1, 99.9, by = 0.1))
    
    weight_WHO_percentile <- WHOCharts[WHOCharts$Percentile %in% percentile_min, "MEAS"] %>% as.numeric(.)
    
    if (max(growthChartsTable$TP) >= (weight_WHO_percentile/1000)) {
      Percentile_PMA <- growthChartsTable %>% filter(TP < (weight_WHO_percentile)/1000) %>% slice(n())
      Percentile_PMA <- Percentile_PMA$Day + offset
    }
  }
  weight_transition_WHO <-
    ifelse(is.na(percentile_min), 
           "No trajectory percentile value available",
           ifelse(is.na(Percentile_PMA),
                  paste0((44*7+offset)%/%7," weeks ", (44*7+offset)%%7, " days PMA (Size for percentile not attainable)"),
                  paste0(weight_WHO_percentile, " grams (", Percentile_PMA %/% 7, "w", Percentile_PMA %%7, "d PMA)")))
  return(weight_transition_WHO)
}


#* Retrieve GA- and Sex-specific weight plots
#* @param GA
#* @param SEX
#* @param offset
#* @param DOL
#* @param VALUE
#* @param FROM
#* @param TO
#* @param width
#* @param height
#* @post /plot
#* @serializer contentType list(type="application/pdf")
function(GA, SEX, TYPE, offset, DOL = integer(0), VALUE = integer(0), FROM = 0, TO = integer(0), width = 8, height = 6) {
  
  if (length(DOL) > 0 & length(VALUE) > 0 & length(DOL) == length(VALUE)) {
    GA <- as.numeric(GA)
    offset <- as.numeric(offset)

    library(tidyverse)
    
    growthChartsTable <- growthCharts(GA, SEX, TYPE)
    percentile_min <- TP(GA, SEX, TYPE, DOL, VALUE, FROM, TO)
    
    TO <- ifelse(length(TO) == 0, (44-GA)*7, TO)
    
    percentile_df <- df(GA, SEX, TYPE, DOL, VALUE)
    percentile_df <- percentile_df[percentile_df$Daily_DSB %in% c(FROM:TO),]
    
    percentile_df <- percentile_df %>%
      right_join(growthChartsTable %>% select(Daily_DSB, Day, Predicted_expected, sigma))
    
    
    growthChartsTable$TP <- (growthChartsTable$Predicted_expected + growthChartsTable$sigma * qnorm(percentile_min/100))
    
    plot <- 
      ggplot(growthChartsTable, aes(x = (Day + offset)/7)) + 
      geom_line(aes(y = percentile_3_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.75, color = "#546E7A", linetype = "solid") +
      geom_line(aes(y = percentile_10_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.5, color = "#546E7A", linetype = "dashed") +
      geom_line(aes(y = percentile_25_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.5, color = "#546E7A", linetype = "dotted") +
      geom_line(aes(y = percentile_50_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.5, color = "#546E7A", linetype = "solid") +
      geom_line(aes(y = percentile_75_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.5, color = "#546E7A", linetype = "dotted") +
      geom_line(aes(y = percentile_90_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.5, color = "#546E7A", linetype = "dashed") +
      geom_line(aes(y = percentile_97_var * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.75, color = "#546E7A", linetype = "solid") +
      geom_point(data = percentile_df %>% filter(!is.na(VALUE)), aes(x = (Day + offset)/7, y = VALUE), color = "#CC0000", size = 3) +
      geom_line(mapping = aes(y = TP * ifelse(TYPE %in% "Weight", 1000, 1)), size = 0.75, color = "#F1C231") +
      labs(x = "Postmenstrual Age (week)",
           y = ifelse(TYPE %in% "Weight", "Weight (gram)", ifelse(TYPE %in% "Length", "Length (cm)", "Head Circumference (cm)")),
           title = NULL) +
      scale_x_continuous(breaks = 2 * c(10:30), sec.axis = sec_axis(~ (. - (GA * 7 + offset)/7), breaks =c(0:21), name = "Week of Life")) +
      scale_y_continuous(breaks = ifelse(TYPE %in% "Weight", 500, 2) * c(0:50)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12, color = "black"),
            axis.title.x = element_text(size = 14, hjust = 1, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold", margin = margin(0, 0.1, 0, 0, "inch")),
            plot.margin = margin(0, 0, 0, 0, "cm"),
            plot.subtitle = element_text(size = 14, hjust = 0.5, colour = "#a57164", face = "bold"))
    
    tmp <- tempfile()
    pdf(tmp, width = as.numeric(width), height = as.numeric(height))
    print(plot)
    dev.off()
    
    readBin(tmp, "raw", n=file.info(tmp)$size)
  }
}