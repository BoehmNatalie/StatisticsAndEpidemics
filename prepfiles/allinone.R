#### some imports are not relevant
library(dplyr)
library(Matrix)
library(scales)
library(progress)
library(pbapply)
library(tidyverse)
library(runner)
library(RColorBrewer) 
library(epinowcast)

###############################################################################
###############################################################################
######################## execution functions from #############################
##https://github.com/kassteele/Nowcasting
###############################################################################
###############################################################################

kassteelen_einfach <- function(df,meandelay,maxdelay,nowcast_start,while_end_date,startdate,daysback,locationfilter){
  blue <- brewer.pal(n = 9, name = "Set1")[2]
  oran <- brewer.pal(n = 9, name = "Set1")[5]
  grey <- brewer.pal(n = 9, name = "Set1")[9]
  source("functions/genPriorDelayDist.R")
  
  
  
  f.priordelay <- genPriorDelayDist(mean.delay = meandelay, max.delay = maxdelay, p = 0.99)
  nowcast.date <- as.Date(nowcast_start)
  
  df_ch <- df %>%
    filter(as.character(df$geoRegion) == locationfilter)
  source("prepfiles/Kassteelen.R")
  epi.data <- load_data_kassteelen(df_ch)
  
  epi.data <- epi.data %>%
    mutate(
      onset.date  = as.Date(onset.date),  
      report.date = as.Date(report.date)  
    )
  df_list <- list()
  i <- 1
  while (nowcast.date <= as.Date(while_end_date)) {
    print(nowcast.date)
    source("functions/dataSetup.R")
    
    rep.data <- dataSetup(
      data         = epi.data,
      start.date   = as.Date(startdate), # Starting date of outbreak
      end.date     = nowcast.date, # Ending date of outbreak (in real-time, leave NULL so end.date = nowcast.date)
      nowcast.date = nowcast.date,          # Nowcast date
      days.back    = daysback,                  # Number of days back from nowcast.date to include in estimation procedure
      f.priordelay = f.priordelay)          # Prior delay PMF
    
    source("functions/modelSetup.R")
    
    model.setup <- modelSetup(
      data = rep.data,
      ord = 2,
      kappa = list(u = 1e6, b = 1e6, wprn = 0.01, s = 1e-6))
    
    source("functions/nowcast.R")
    
    nowcast.list <- nowcast(data = rep.data, model = model.setup, conf.level = 0.90)
    
    
    ### with that a nowcast can be plot in each iteration
    #source("functions/plotNowcast.R")
    
    #nowcast.plot <- plotNowcast(data = rep.data, nowcast = nowcast.list, title = "Nowcast")
    #plot(nowcast.plot)
    
    temp_df <- nowcast.list[["nowcast"]] %>%
      mutate(
        forecast_date = nowcast.date,
        location = locationfilter  
      )
    
    
    df_list <- bind_rows(df_list, temp_df)
    
    nowcast.date <- nowcast.date + 1
    i <- i+1
  }
  
  source("prepfiles/truth_erstellen.R")
  df_e <- df %>%
    filter(as.character(df$geoRegion) == locationfilter)
  
  df_truth <- truth_erstellen(df_e,maxdelay)

  df_truth$date <- as.Date(df_truth$date, format = "%Y-%m-%d")  
  
  df_truth <- as.data.frame(df_truth) 
  df_truth <- df_truth %>%
    ungroup() %>%                 
    select(date, truth)  
  df_truth <- df_truth%>%
    rename(
      target_end_date = date
    )
  df_list <- as.data.frame(df_list) 
  df_list <- df_list%>%
    rename(
      target_end_date = Date
    )
  
  
  df_list$target_end_date <- as.Date(df_list$target_end_date)
  
  result_full <- df_truth %>%
    left_join(df_list, by = "target_end_date")
  
  min_date <- min(df_list$target_end_date, na.rm = TRUE)
  max_date <- max(df_list$target_end_date, na.rm = TRUE)
  
  filtered_df <- result_full %>%
    filter(target_end_date >= as.Date(min_date) & target_end_date <= as.Date(max_date))
  
  #################### generate table needed for score ####################
  
  data <- filtered_df %>%
    mutate(
      pathogen = "COVID-19",       
      model = "Kassteele",         
      retrospective = FALSE        
    ) %>%
    rename(
      median = med,  
      q5 = lwr,    
      q95 = upr    
    )
  
  modified_summary <- data %>%
    gather(key = "quantile", value = "value", starts_with("q")) %>%
    mutate(
      type = "quantile"
    ) %>%
    mutate(quantile = as.numeric(gsub("q", "", quantile)) / 100) %>%  
    select(location, forecast_date, target_end_date, type, quantile, value, pathogen, model, retrospective, truth)
  
  modified_summary_median <- data %>%
    mutate(
      type = "median",  
      quantile = 0.5, 
      value = median, 
    ) %>%
    select(location, forecast_date, target_end_date, type, quantile, value, pathogen, model, retrospective,truth)
  
  modified_summary <- modified_summary %>%
    mutate(quantile = as.numeric(quantile))
  final_summary <- bind_rows(modified_summary, modified_summary_median)
  
  
  #################### compute score ####################
  
  ergebnis <- compute_scores(final_summary)
  
  #################### compute WIS ####################
  
  wis_ergebnis <- compute_wis(final_summary)
  
  df_nowcast <- df_list
  df_truth_filtered <- df_truth %>%
    filter(target_end_date >= as.Date(startdate) & target_end_date <= as.Date(while_end_date))
  
  return(list(ergebnis=ergebnis,wis=wis_ergebnis,final_summary=final_summary,df_truth_filtered =df_truth_filtered,df_nowcast = df_nowcast))
  
}


###############################################################################
###############################################################################
########## score computation according to ##########
#https://github.com/dwolffram/hospitalization-nowcast-hub-evaluation
###############################################################################
###############################################################################

compute_scores <- function(df) {
  df <- df %>%
    rowwise() %>%
    mutate(score = score(value, truth, type, quantile),
           score = round(score, digits = 5)) %>% 
    select(-c(pathogen, value, truth))
}
compute_wis <- function(df) {
  df_median <- df %>%
    filter(quantile == 0.5) %>%
    rename(med = value) %>%
    select(-any_of(c("quantile", "pathogen", "retrospective", "truth")))

  df_median <- df_median %>%
    filter(type == "quantile")
  
  df <- df %>%
    filter(type == "quantile") %>%
    left_join(df_median, by = c("forecast_date", "target_end_date","location"))    %>%
    select(
      forecast_date,
      target_end_date,
      location,
      type = type.x,  
      quantile,
      value,
      pathogen,
      retrospective,
      model = model.x,
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
      spread = mean(spread),
      overprediction = mean(overprediction),
      underprediction = mean(underprediction),
      score = mean(score)
    )
  
  return(df)
}
score <- function(prediction, observation, type, quantile) {
  if (type == "mean") {
    if (is.na((prediction - observation)^2)){

    }

    return((prediction - observation)^2)
  } else if (type == "median") {
    if (is.na(abs(prediction - observation))){

    }
    return(abs(prediction - observation))
  } else if (type == "quantile") {
    if (is.na(abs(prediction - observation))){

    }
    return(qs(prediction, observation, quantile))
  }
}
qs <- function(q, y, alpha) {
  2 * (as.numeric(y < q) - alpha) * (q - y)
}
