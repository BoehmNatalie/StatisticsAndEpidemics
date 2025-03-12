################################################################################
################################################################################
################################################################################
############################# Plotfunctions ####################################
########## Plotfunctions and score calculations from
########## https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation/tree/main/code 
################################################################################
################################################################################
################################################################################
source("prepfiles/allinone.R")
plot_wis <- function(df,month,monat_filtern,title) {
  if (monat_filtern ==TRUE){
    df$forecast_date <- ymd(df$forecast_date)
    
    df <- df %>% filter(month(forecast_date) == month & year(forecast_date) == 2021)
  }
  wis_ergebnis <- compute_wis_mehrere(df)
  df<-wis_ergebnis
  scores <- wis_ergebnis %>%
    select(-score) %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")
  
  ggplot() +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") + 
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.15, "lines"), 
      size =15 / .pt,
      label.padding = unit(0.1, "lines") 
    ) +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    scale_x_discrete(limits = rev, drop = FALSE) +
    guides(color = "none", fill = "none") +
    labs(
      #title = title, 
      x =NULL,
      y = title,
      color = "Model",
      alpha = "Decomposition of WIS"
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
}
plot_coverage <- function(df, month, monat_filtern, title) {
  #filter months
  if (monat_filtern == TRUE) {
    df$forecast_date <- ymd(df$forecast_date)
    df <- df %>% filter(month(forecast_date) == month & year(forecast_date) == 2021)
  }
  
  if (!"quantile" %in% names(df) | !"value" %in% names(df)) {
    stop("Die Spalten 'quantile' oder 'value' fehlen in df!")
  }
  
  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_") 
  
  if (!"truth" %in% names(df_wide)) {
    stop("Die Spalte 'truth' fehlt in df!")
  }
  
  df_wide <- df_wide %>%
    mutate(
      c60 = (truth >= quantile_0.2 & truth <= quantile_0.8),
      c90 = (truth >= quantile_0.05 & truth <= quantile_0.95)
    )
  
  coverage_df <- df_wide %>%
    group_by(model) %>%
    summarize(
      c60 = mean(c60, na.rm = TRUE),
      c90 = mean(c90, na.rm = TRUE)
    )
  
  alphas <- setNames(c(0.7, 0.4), c("60%", "90%"))
  
  ggplot(coverage_df, aes(x = model)) +
    expand_limits(y = 1) +
    geom_col(aes(y = c90, fill = model, alpha = "90%")) +
    geom_col(aes(y = c60, fill = model, alpha = "60%")) +
    geom_hline(yintercept = c(0.6, 0.90), linetype = "dashed") +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      x = NULL,
      y = title,
      color = "Model",
      alpha = "Prediction \ninterval"
    ) +
    coord_flip() +
    scale_x_discrete(limits = rev, drop = FALSE) +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    theme(legend.position.inside = c(0.9, 0.35), legend.justification = c(1, 1), legend.box.just = "left")
}
plot_wis_month <- function(df,month,monat_filtern,title) {
  if (monat_filtern ==TRUE){
    df$forecast_date <- ymd(df$forecast_date)
    df <- df %>% filter(month(forecast_date) == month & year(forecast_date) == 2021)
  }
  wis_ergebnis <- compute_wis_mehrere(df)
  df<-wis_ergebnis
  scores <- wis_ergebnis %>%
    select(-score) %>%
    pivot_longer(cols = c(underprediction, spread, overprediction), names_to = "penalty")
  #change Reihenfolge
  df$model <- factor(df$model, levels = c(
    "Epinowcast_02", "Kassteele_02",
    "Epinowcast_03", "Kassteele_03",
    "Epinowcast_04", "Kassteele_04",
    "Epinowcast_05", "Kassteele_05",
    "Epinowcast_06", "Kassteele_06"
  ))
  ggplot() +
    geom_bar(data = df, aes(x = model, y = score), fill = "white", stat = "identity") +
    geom_bar(data = scores, aes(x = model, y = value, fill = model, alpha = penalty, color = model), size = 0.1, stat = "identity") +
    geom_label(
      data = df, aes(x = model, y = 0.5 * score, label = sprintf("%0.1f", round(score, digits = 1))),
      fill = "white", alpha = 1, hjust = 0.5,
      label.r = unit(0.15, "lines"), 
      size =15 / .pt,
      label.padding = unit(0.1, "lines") 
    ) +
    scale_alpha_manual(
      values = c(0.5, 0.2, 1), labels = c("Overprediction", "Spread", "Underprediction"),
      guide = guide_legend(reverse = TRUE, title.position = "top", title.hjust = 0.5)
    ) +
    scale_x_discrete(limits = rev, drop = FALSE) +
    guides(color = "none", fill = "none") +
    labs(
      #title = title,  # titel doch nicht hier erzeugen
      x =NULL,
      y = title,
      color = "Model",
      alpha = "Decomposition of WIS"
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
}
plot_coverage_month <- function(df, month, monat_filtern, title) {
  
  if (monat_filtern == TRUE) {
    df$forecast_date <- ymd(df$forecast_date)
    
    df <- df %>% filter(month(forecast_date) == month & year(forecast_date) == 2021)
  }
  
  if (!"quantile" %in% names(df) | !"value" %in% names(df)) {
    stop("Die Spalten 'quantile' oder 'value' fehlen in df!")
  }
  
  df_wide <- df %>%
    pivot_wider(names_from = quantile, values_from = value, names_prefix = "quantile_") 
  
  if (!"truth" %in% names(df_wide)) {
    stop("Die Spalte 'truth' fehlt in df!")
  }
  
  df_wide <- df_wide %>%
    mutate(
      c60 = (truth >= quantile_0.2 & truth <= quantile_0.8),
      c90 = (truth >= quantile_0.05 & truth <= quantile_0.95)
    )
  
  coverage_df <- df_wide %>%
    group_by(model) %>%
    summarize(
      c60 = mean(c60, na.rm = TRUE),
      c90 = mean(c90, na.rm = TRUE)
    )
  
  #change Reihenfolge
  coverage_df$model <- factor(coverage_df$model, levels = c(
    "Epinowcast_02", "Kassteele_02",
    "Epinowcast_03", "Kassteele_03",
    "Epinowcast_04", "Kassteele_04",
    "Epinowcast_05", "Kassteele_05",
    "Epinowcast_06", "Kassteele_06"
  ))
  
  
  alphas <- setNames(c(0.7, 0.4), c("60%", "90%"))
  
  ggplot(coverage_df, aes(x = model)) +
    expand_limits(y = 1) +
    geom_col(aes(y = c90, fill = model, alpha = "90%")) +
    geom_col(aes(y = c60, fill = model, alpha = "60%")) +
    geom_hline(yintercept = c(0.6, 0.90), linetype = "dashed") +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    labs(
      x = NULL,
      y = title,
      color = "Model",
      alpha = "Prediction \ninterval"
    ) +
    coord_flip() +
    scale_x_discrete(limits = rev, drop = FALSE) +
    guides(fill = "none") +
    scale_alpha_manual(values = alphas) +
    theme_bw() +
    theme(legend.position.inside = c(0.9, 0.35), legend.justification = c(1, 1), legend.box.just = "left")
}
filter_single_forecast_rename <- function(df,dayfilter){
  df <- df %>%
    rename(forecast_date = date_report)
  df <- df %>%
    rename(target_end_date = date_hospitalization)
  df <- df %>%
    mutate(
      days_difference = as.numeric(difftime(target_end_date, forecast_date, units = "days"))
    )
  df <- df %>%
    filter(days_difference==dayfilter)
  return (df)
}
filter_single_forecast <- function(df,dayfilter){
  df <- df %>%
    mutate(
      days_difference = as.numeric(difftime(target_end_date, forecast_date, units = "days"))
    )
  df <- df %>%
    filter(days_difference==dayfilter)
  return (df)
}
compute_wis_mehrere <- function(df) {
  unique_dates <- unique(df$forcast_date)
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    rename(med = value) %>%
    select(-any_of(c("quantile", "pathogen", "retrospective", "truth")))
  df_median <- df_median %>%
    filter(type == "quantile")
  
  df <- df %>%
    filter(type == "quantile") %>%
    left_join(df_median, by = c("forecast_date", "target_end_date","location","model"))    %>%
    select(
      forecast_date,
      target_end_date,
      location,
      type = type.x,  
      quantile,
      value,
      pathogen,
      retrospective,
      model = model,
      truth,
      med,
    )
  df <- df %>%
    rowwise() %>%
    mutate(
      score = score(value, truth, type, quantile),
      spread = score(value, med, type, quantile),
      overprediction = ifelse(med > truth, score - spread, 0),
      underprediction = ifelse(med < truth, score - spread, 0)
    )
  df_safe<-df
  
  
  df <- df %>%
    group_by(model) %>%
    summarize(
      target_end_date = unique_dates,  
      spread = mean(spread),
      overprediction = mean(overprediction),
      underprediction = mean(underprediction),
      score = mean(score)
    )
  
  return(df)
}
####### all reports for one day
truth_maximal <- function(df,locationfilter){
  df <- df %>%
    filter(geoRegion==locationfilter)
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
    mutate(value_0d = replace_na(value_0d, 0))
  
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
###### reported hospi within maximal reporting delay
truth_compute <- function(df,locationfilter,max_delay){
  df <- df %>%
    filter(geoRegion==locationfilter)
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
    filter(delay<=max_delay)
  
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
plot_nowcast_for_one_horizon <-function(df_truth_filtered,df_nowcast,df_truth_max){
  plot <-ggplot(df_truth_filtered) +
    geom_ribbon(
      data = df_nowcast,
      aes(x = target_end_date, ymin = q5, ymax = q95), fill = "lightblue" , 
      alpha = 0.8
    ) +
    geom_ribbon(
      data = df_nowcast,
      aes(x = target_end_date, ymin = q20, ymax = q80), fill = "skyblue",
      alpha = 0.95
    ) +
    
    geom_line(
      data = df_nowcast, 
      aes(x = target_end_date, y = median), color = "skyblue3",    
      linewidth = 0.6
    ) +
    geom_line(
      aes(x = forecast_date, y = total), color = "black",  
      linewidth = 0.6
    ) +
    geom_line(
      data = df_truth_max,
      aes(x = date, y = truth), color = "red",    
      linewidth = 0.6
    )+
    
    
    labs(
      x = NULL,
      y = "hospitalization",
    ) +
    
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "#f4f4f4"), 
      plot.background = element_rect(fill = "white")  
    )
  print(plot)
}