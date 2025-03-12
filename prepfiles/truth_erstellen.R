
truth_erstellen <- function(df,max_delay){
  max_delay <- max_delay
  df <- df %>%
    rename(date_case = date_hospitalization)
  
  df <- df %>%
    group_by(geoRegion, date_case, date_report) %>%  
    summarise(total = sum(total), .groups = "drop") %>%
    arrange(geoRegion, date_case, date_report)  

  df <- df %>%
    mutate(
      date_case = as.Date(date_case),       
      date_report = as.Date(date_report)    
    )

  df <- df %>%
    mutate(delay = as.numeric(date_report - date_case))  
  df <- df %>%
    filter(delay <= max_delay)  
  
 
  df <- df %>%
    arrange(geoRegion, date_case) %>%  
    mutate(
      value_0d = if_else(
        date_case == lag(date_case, default = first(date_case)), 
        total - lag(total, default = 0),                         
        total                                                   
      )
    ) %>%
    ungroup()
  df <- df %>%
    filter(!is.na(value_0d))
  
  df <- df %>% select(-total,-delay, -date_report)
  
  df_truth <- df %>%
    group_by(geoRegion, date_case) %>%
    summarize(total_value_0d = sum(value_0d, na.rm = TRUE))

  df_truth <- df_truth %>%
    ungroup() %>%  
    rename(truth = total_value_0d,
           date = date_case,
           location = geoRegion)
  return(df_truth)
}