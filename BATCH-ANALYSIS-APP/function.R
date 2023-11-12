loadGrowthCharts <- function(table, type = c("Weight", "Length", "HeadCircumference")) {
  require(duckdb)
  require(DBI)
  
  GAn_Sex <- unique(table %>% select(GAn, Sex))
  GTC_dt <- data.frame()
  con <- dbConnect(duckdb::duckdb(), dbdir="../DATABASE/db.duckdb", read_only=TRUE)
  for (i in c(1: nrow(GAn_Sex))) {
    temp <- dbGetQuery(con, paste0("SELECT * FROM GA",GAn_Sex[[i,"GAn"]],"_", GAn_Sex[[i,"Sex"]],"_",type)) %>% 
      mutate(percentile = as.numeric(NA), GAn = GAn_Sex[[i,"GAn"]], Sex = GAn_Sex[[i,"Sex"]])
    GTC_dt <- rbind(GTC_dt, temp)
  }
  dbDisconnect(con, shutdown = TRUE)
  
  return(GTC_dt)
}

calculate_Z <- function(data = NULL, type = NULL) {
  factor <- ifelse(type == "Weight", 1000, 1)
  selected_col <- c("ID", "Sex", "GA_week", "GA_day", "GAn", "DOL", "Day", paste0("_", type), paste0("_", type, " Z"), "_Group")
  table <- data %>% 
    select(any_of(selected_col))

  if (paste0("_", type) %in% colnames(table)) {
    table <- table %>%
      mutate(GAn = round((GA_week * 7 + GA_day)/7),
             Day = GAn * 7 + DOL) 
    chou <- loadGrowthCharts(table, type)
    table <- table %>% left_join(chou)
    table[[paste0("_", type, " Z")]] <- round((table[[paste0("_", type)]] - table$Predicted_expected*factor) / (table$sigma*factor), digits = 3)
  }
  
  return(table)
}

findPercentile <- function(table, type, factor) {
  grid <- seq(0.1, 99.9, by = 0.1)
  RMQElowest <- NA
  lowestPercentile <- NA
  if (paste0("_", type) %in% colnames(table)) {
    table["type"] <- table[paste0("_", type)]
    table["type_Z"] <- table[paste0("_", type, " Z")]
    table <- table %>% filter(!is.na(type_Z))
    if (nrow(table)>0) {
      for (i in c(1:length(grid))) {
        b <- table %>% 
          mutate(percentile = grid[i],
                 expectedForPercentile = factor * (Predicted_expected + sigma * qnorm(grid[i]/100)),
                 residual = type - expectedForPercentile)
        RMQE <- sqrt(sum(b$residual^2))
        RMQElowest <- ifelse(is.na(RMQElowest) | RMQE <= RMQElowest, RMQE, RMQElowest)
        lowestPercentile <- ifelse(RMQE == RMQElowest, i, lowestPercentile)
      }   
    }
  }
  if (is.na(lowestPercentile)) {
    return(NA)
    } else {
      return(grid[lowestPercentile])
      }
}

functionforDoParallel <- function(table, type, factor, colors, i) {
  temp <- table %>% filter(ID %in% unique(table$ID)[i])
  percentile <- findPercentile(temp, type, factor)
  group <- unique(temp[["_Group"]])
  temp_df <- data.frame(ID = as.character(unique(table$ID)[i]), Percentile = as.numeric(percentile), Group = as.character(group))
  return(temp_df)
}

FindPercentileForAll <- function(table, type, factor, colors, id) {
  require(foreach)
  require(doParallel)
  
  df_list_all <- list()
  
  num_cores <- detectCores()
  registerDoParallel(cores=num_cores - 1)
  
  allID <- unique(table$ID)
  allIDNumber <- length(allID)
  IDBracket <- c(1:ceiling(allIDNumber/1000))
  if (!file.exists(id)) {
  dir.create(id)
  }
  files <- list.files(id, full.names = TRUE)
  file.remove(files)
  
  for (bracket in IDBracket) {
    IDFilterIn <- allID[c((1+1000*(bracket-1)):(1000+1000*(bracket-1)))]
    IDFilterIn <- IDFilterIn[!is.na(IDFilterIn)]
    write_csv(table %>% filter(ID %in% IDFilterIn), paste0(id, "/temp_",bracket,".csv"))
  }
  
  withProgress(value = 0, message = paste0("Calculating ", type, " Trajectory Percentile"), {
  
  for (bracket in IDBracket) {
    temp_table <- read_csv(paste0(id,"/temp_",bracket,".csv"))

    #IDFilterIn <- allID[c((1+1000*(bracket-1)):(1000+1000*(bracket-1)))]
    #IDFilterIn <- IDFilterIn[!is.na(IDFilterIn)]
    #table_subset <- table %>% filter(ID %in% IDFilterIn)

    incProgress(1/max(IDBracket), detail = paste0("Now processing ID batch ", bracket))
    df_list <- foreach(i = c(1:length(unique(temp_table$ID)))) %dopar% {
      functionforDoParallel(temp_table, type, factor, colors, i)
    }
    df_list <- as.list(df_list)
    df_list_all <- c(df_list_all, df_list)
  }
    
    unlink(id, recursive = TRUE)
    
  
  })

  
    
  
  stopImplicitCluster()
  
  df <- data.frame()
  
  for (i in c(1:length(df_list_all))) {
    df <- rbind(df, df_list_all[[i]])
  }
  
  colnames(df)[colnames(df) == "Percentile"] <- paste0("_", type, " Percentile")
  return(df)
}

# FindPercentileForAll <- function(table, type, factor, colors) {
#   df <- data.frame()
#   withProgress(value = 0, message = paste0("Calculating ", type, " Trajectory Percentile"), {
#     for (i in c(1:length(unique(table$ID)))) {
#       incProgress(1/length(unique(table$ID)), detail = paste0("Now processing ID ", unique(table$ID)[i]))
#       temp <- table %>% filter(ID %in% unique(table$ID)[i])
#       percentile <- findPercentile(temp, type, factor)
#       group <- unique(temp[["_Group"]])
#       temp_df <- data.frame(ID = unique(table$ID)[i], Percentile = percentile, Group = group)
#       df <- rbind(df, temp_df)
#     }
#   })
# 
#   colnames(df)[colnames(df) == "Percentile"] <- paste0("_", type, " Percentile")
#   
#   return(df)
# }

reset_graph_box <- function(session, input, output) {
  updateSelectInput(session, "weight_type_analysis", selected = "")
  updateSelectInput(session, "length_type_analysis", selected = "")
  updateSelectInput(session, "HC_type_analysis", selected = "")
  
  output$weight_UI <- renderUI({})
  output$length_UI <- renderUI({})
  output$HC_UI <- renderUI({})
  
}
