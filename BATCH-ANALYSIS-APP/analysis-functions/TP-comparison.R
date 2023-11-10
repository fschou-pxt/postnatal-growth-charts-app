TP_comparison <- function(table, type, colors = list(), font) {
  table <- table %>%
    rename(value = paste0("_", type, " Percentile"))
  plot <- 
    ggplot(table, aes(x = `_Group`, y = value)) +
#    geom_point(aes(color = `_Group`)) +
    geom_dotplot(aes(group = `_Group`, fill = `_Group`, color = `_Group`), binaxis = "y", stackdir = "center", binwidth = 0.25) +
    geom_boxplot(aes(group = `_Group`)) +
    labs(x = "\nGroup", y = "Trajectory Percentile", title = "Trajectory Percentile by Group") +
    scale_y_continuous(limits = c(0, 100), breaks = 10* c(0:10)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12, color = "black", family = font),
          axis.title.x = element_text(size = 12, face = "bold", family = font),
          axis.title.y = element_text(size = 12, face = "bold", family = font),
          strip.background = element_rect(fill = colors$color3),
          strip.text = element_text(size = 14, color = "white", family = font),
          plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"),
          legend.position = "none")
  
  if (length(unique(table$`_Group`)) == 1) {
    pvalue1 = paste0("<span style='background-color:", colors$color7, ";'><em>Only 1 group.</em></span>")
    method1 = ""
    pvalue2 = paste0("<span style='background-color:", colors$color7, ";'><em>Only 1 group.</em></span>")
    method2 = ""
  } else if (length(unique(table$`_Group`)) == 2) {
    dataNumber <-
      table %>% group_by(`_Group`) %>%
      summarise(n=n())
    if (all(dataNumber$n > 1)) {
      pvalue1 = t.test(value ~ `_Group`, table, var.equal = FALSE)$p.value
      pvalue1 = ifelse(pvalue1 < 0.001, "<0.001", round(digits= 3, pvalue1))
      method1 = "Welch two-sample t-test"
      pvalue2 = wilcox.test(value ~ `_Group`, table)$p.value
      pvalue2 = ifelse(pvalue2 < 0.001, "<0.001", round(digits= 3, wilcox.test(value ~ `_Group`, table)$p.value))
      method2 = "Wilcoxon rank sum test"
    } else {
      pvalue1 = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
      method1 = ""
      pvalue2 = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
      method2 = ""
    }
  } else if (length(unique(table$`_Group`)) > 2) {
    dataNumber <-
      table %>% group_by(`_Group`) %>%
      summarise(n=n())
    if (all(dataNumber$n > 1)) {
      pvalue1 = oneway.test(value ~ `_Group`, table)$p.value
      pvalue1 = ifelse(pvalue1 < 0.001, "<0.001", round(digits= 3, oneway.test(value ~ `_Group`, table)$p.value))
      method1 = "One-way ANOVA"
      pvalue2 = kruskal.test(value ~ `_Group`, table)$p.value
      pvalue2 = ifelse(pvalue2 < 0.001, "<0.001", round(digits= 3, kruskal.test(value ~ `_Group`, table)$p.value))
      method2 = "Kruskal-Wallis test"
    } else {
      pvalue1 = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
      method1 = ""
      pvalue2 = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
      method2 = ""
    }
  }
  
  
  summary <- table %>%
    group_by(`_Group`) %>%
    summarise(
      `Number of infants` = n(),
      `mean ± sd` = paste0(round(digits = 1, mean(value, na.rm = T)), " ± ", round(digits = 1, sd(value, na.rm = T))),
      `median (IQR)` = paste0(round(digits = 1, median(value, na.rm = T)), " (", paste0(collapse = ", ", round(digits = 1, quantile(value, probs = c(0.25, 0.75), na.rm = T))) , ")")
      ) %>% 
    column_to_rownames(var = "_Group") %>%
    t(.) %>% as.data.frame(.) %>%
    rownames_to_column(var = "_Group") 
  summary$`P value` <- c("", pvalue1, pvalue2)
  summary <- summary %>%
    gt::gt() %>%
    fmt_markdown(columns = vars(`P value`)) %>%
    #tab_source_note(HTML(paste0("P value<sup>1</sup>: ", ifelse(is.na(as.numeric(pvalue)), pvalue, round(digits = 3, pvalue))))) %>%
    tab_source_note(HTML(paste0("<sup>1</sup>Method: ", method1))) %>%
    tab_source_note(HTML(paste0("<sup>2</sup>Method: ", method2))) %>%
    tab_header("Trajectory Percentile Summary by Group") %>%
    tab_options(
      table.font.size = 12
    )

  return(list(summary,plot))
}