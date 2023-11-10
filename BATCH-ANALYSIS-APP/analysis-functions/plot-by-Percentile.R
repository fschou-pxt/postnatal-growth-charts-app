plot_by_Percentile <- function(table, type, colors = list(), font = "Nunito") {

  #table$value <- table[paste0("_",type," Z")]
  columnToRename <- paste0("_",type," Z")
  table <- table %>%
    rename(value = columnToRename)
  
  plot <-
    ggplot(table, aes(x = DOL/7, y = pnorm(value)*100)) +
    geom_point(aes(color = `_Group`)) +
    geom_line(aes(color = `_Group`, group = ID)) +
    labs(x = "\nWeek of life", y = "Percentile", title = NULL) +
    scale_x_continuous(limits = c(0, 17), breaks = 2* c(0:100)) +
    scale_y_continuous(limits = c(0, 100), breaks = 10* c(0:10)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12, color = "black", family = font),
          axis.title.x = element_text(size = 12, face = "bold", family = font),
          axis.title.y = element_text(size = 12, face = "bold", family = font),
          strip.background = element_rect(fill = colors$color3),
          strip.text = element_text(size = 14, color = "white", family = font),
          plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"),
          legend.position = "bottom")
  
  return(plot)
}