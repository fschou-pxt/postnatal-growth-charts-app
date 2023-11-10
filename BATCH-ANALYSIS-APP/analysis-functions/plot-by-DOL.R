plot_by_DOL <- function(table, type, colors = list(), font = "Nunito") {
  GAn_Sex <- unique(table %>% select(GAn, Sex))
  factor <- NA
  y_title <- ""
  ## Prepare type variables
  if (type == "Weight") {
    factor = 1000
    y_title = "Weight (gram)\n"
  } else if (type == "Length") {
    factor = 1
    y_title = "Length (cm)\n"
  } else if (type == "HeadCircumference") {
    factor = 1
    y_title = "Head Circumference (cm)\n"
  }
  
  ## retrieve growth charts
  require(duckdb)
  require(DBI)
  GTC_dt <- data.frame()
  con <- dbConnect(duckdb::duckdb(), dbdir="~/Apps/GTC-Website-Apps/GTC-DB/db.duckdb", read_only=TRUE)
  for (i in c(1: nrow(GAn_Sex))) {
    temp <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn_Sex[[i,"GAn"]],"_", GAn_Sex[[i,"Sex"]],"_",type)) %>% 
      mutate(percentile = as.numeric(NA), GAn = GAn_Sex[[i,"GAn"]], Sex = GAn_Sex[[i,"Sex"]])
    GTC_dt <- rbind(GTC_dt, temp)
  }
  dbDisconnect(con, shutdown = TRUE)
  
  GTC_dt <- GTC_dt %>%
    rename(DOL = "Daily_DSB") %>%
    mutate(Plot_Group = paste0(Sex, ".", GAn))
  
  table <- table %>%
    mutate(Plot_Group = paste0(Sex, ".", GAn)) %>%
    rename(value = paste0("_",type),
           value_z = paste0("_", type, " Z")) %>%
    filter(!is.na(value_z), !(value %in% 0))

  plot <-
  ggplot(GTC_dt, aes(x = (DOL/7))) +
    geom_point(data = table, mapping = aes(y = value, color = `_Group`), size = 0.25) +
    geom_line(data = table, mapping = aes(y = value, group = ID, color = `_Group`), size = 0.25) +
    geom_line(aes(y = percentile_3_var * factor), size = 0.75, color = colors$color2, linetype = "solid") +
    geom_line(aes(y = percentile_10_var * factor), size = 0.5, color = colors$color2, linetype = "dashed") +
    geom_line(aes(y = percentile_25_var * factor), size = 0.5, color = colors$color2, linetype = "dotted") +
    geom_line(aes(y = percentile_50_var * factor), size = 0.5, color = colors$color2, linetype = "solid") +
    geom_line(aes(y = percentile_75_var * factor), size = 0.5, color = colors$color2, linetype = "dotted") +
    geom_line(aes(y = percentile_90_var * factor), size = 0.5, color = colors$color2, linetype = "dashed") +
    geom_line(aes(y = percentile_97_var * factor), size = 0.75, color = colors$color2, linetype = "solid") +
    labs(x = "\nWeek of life", y = y_title, title = NULL) +
    scale_x_continuous(breaks = 4 * c(0:10)) +
    #scale_y_continuous(limits = c(NA, 10000)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12, color = "black", family = font),
          axis.title.x = element_text(size = 12, face = "bold", family = font),
          axis.title.y = element_text(size = 12, face = "bold", family = font),
          strip.background = element_rect(fill = colors$color3),
          strip.text = element_text(size = 14, color = "white", family = font),
          plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"),
          legend.position = "bottom") +
    facet_grid(Sex~GAn, scale = "free_x", space = "free_x")
  
  return(plot)
}