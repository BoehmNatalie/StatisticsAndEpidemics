  ##### Kassteelen Code
  #####
  #transform given Data in form needed

library(dplyr)
library(progress)
load_data_kassteelen <- function(data){
  library(progress)
  data <- data %>%
    group_by(geoRegion, date_hospitalization) %>%
    arrange(date_report) %>%
    mutate(
      new_cases = c(0, diff(total)),  
    ) %>%
    ungroup()
  
  data_sorted <- data[order(data$geoRegion,data$date_hospitalization, data$date_report), ]

  data_sorted$date_hospitalization <- as.Date(data_sorted$date_hospitalization)
  data_sorted$date_report <- as.Date(data_sorted$date_report)
  
  
  df_ohne_na <- na.omit(data_sorted)

  df <- negatives(df_ohne_na)

  


  duplicated_df <- data.frame()
  n=nrow(df)
  pb <- progress_bar$new(total = n, clear = FALSE, width = 60, format = "[:bar] :percent :eta")
  for (i in 1:nrow(df)) {
    pb$tick()

    duplicated_df <- rbind(duplicated_df, df[i, , drop = FALSE][rep(1, as.integer(df$new_cases[i])), , drop = FALSE])
  }
  

  df_dropped <- duplicated_df %>%
    select(-geoRegion, -total, -new_cases)
  
  df_fertig <- df_dropped %>%
    rename(
      onset.date = date_hospitalization,
      report.date = date_report
    )
  return(df_fertig)
}

negatives <- function(df){
  to_subtract <- 0
  for (i in nrow(df):1){
    value <- df[[ncol(df)]][i] + to_subtract
    if (value < 0) {
      to_subtract <- value  
      df[[ncol(df)]][i] <- 0  

    } else {
      df[[ncol(df)]][i] <- value  
      to_subtract <- 0  

    }
  }
  return(df)
}
  
