delta_Z_first_last <- function(table, type, colors = list(), font) {
  Z <- table %>%
    group_by(ID) %>%
    summarise(
      `Number of measurements` = n()
      )
  oneMeasurementID <- Z$ID[Z$`Number of measurements` %in% 1]
  
  Z <- table %>%
    filter(!(ID %in% oneMeasurementID)) %>%
    rename(value = paste0("_", type, " Z")) %>%
    filter(!is.na(value)) %>%
    group_by(ID) %>%
    summarise(
      Group = unique(`_Group`),
      `first Z` = unique(value[DOL == min(DOL)]),
      `last Z` = unique(value[DOL == max(DOL)])
    ) %>%
    ungroup(.) %>%
    mutate(`delta Z` = `last Z` - `first Z`)
  
  plot <- 
    ggplot(Z, aes(x = Group, y = `delta Z`)) +
    geom_boxplot(aes(group = Group)) +
    geom_point(aes(color = Group)) +
    labs(x = "\nGroup", y = "Delta Z Score (Last - First)", title = "Delta Z Score (Last - First) by Group") +
    theme_bw() +
    theme(axis.text = element_text(size = 12, color = "black", family = font),
          axis.title.x = element_text(size = 12, face = "bold", family = font),
          axis.title.y = element_text(size = 12, face = "bold", family = font),
          strip.background = element_rect(fill = colors$color3),
          strip.text = element_text(size = 14, color = "white", family = font),
          plot.margin = margin(0.2, 0.05, 0.1, 0.1, "cm"),
          legend.position = "none")
  
  summary <- data.frame("The table has no content" = character(0)) %>% gt::gt()
  
  if (nrow(Z) > 0) {
    if (length(unique(Z$Group)) == 1) {
      pvalue = paste0("<span style='background-color:", colors$color7, ";'><em>Only 1 group.</em></span>")
      method = ""
    } else if (length(unique(Z$Group)) == 2) {
      dataNumber <-
        Z %>% group_by(Group) %>%
        summarise(n=n())
      if (all(dataNumber$n > 1)) {
        pvalue = t.test(`delta Z` ~ Group, Z)$p.value
        method = "Student's t test"
      } else {
        pvalue = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
        method = ""
      }
    } else if (length(unique(Z$Group)) > 2) {
      dataNumber <-
        Z %>% group_by(Group) %>%
        summarise(n=n())
      if (all(dataNumber$n > 1)) {
        pvalue = oneway.test(`delta Z` ~ Group, Z)$p.value
        method = "ANOVA"
      } else {
        pvalue = paste0("<span style='background-color:", colors$color7, ";'><em>At least 1 group only contains 1 observation.</em></span>")
        method = ""
      }
    }

    summary <- Z %>%
      group_by(Group) %>%
      summarise(
        `Number of infants` = n(),
        `mean ± sd` = paste0(round(digits = 2, mean(`delta Z`, na.rm = T)), " ± ", round(digits = 2, sd(`delta Z`, na.rm = T))),
        `median (IQR)` = paste0(round(digits = 2, median(`delta Z`, na.rm = T)), " (", paste0(collapse = ", ", round(digits = 2, quantile(`delta Z`, probs = c(0.25, 0.75), na.rm = T))) , ")"),
      ) %>% column_to_rownames(var = "Group") %>%
      t(.) %>% as.data.frame(.) %>%
      rownames_to_column(var = "Summary") %>%
      gt::gt() %>%
      tab_source_note(HTML(paste0("P value<sup>1</sup>: ", ifelse(is.na(as.numeric(pvalue)), pvalue, round(digits = 3, pvalue))))) %>%
      tab_source_note(HTML(paste0("<sup>1</sup>Method: ", method))) %>%
      tab_header("Delta Z Score (Last - First) Summary by Group") %>%
      tab_options(
        table.font.size = 12
      )
  }

  
  return(list(Z, plot, summary))
}