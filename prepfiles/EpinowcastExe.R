########### Code zur Erzeugung von Nowcasts mit Epinowcast #####################

library(dplyr)
library(data.table)
library(epinowcast)
library(tidyr)
library(ggplot2)
library(posterior)

source("modelprep.R")

latest_date <- "2021-02-01"
locationfilter <- "CH"
removed_days <-0
included_days <-450
maxed_delay <-36
df<- read.csv("reporting_hospitalizations.csv")

CHcombined_ergebnis_weekday <- data.frame()
CHcombined_wis_weekday <- data.frame()
CHcombined_final_summary_weekday <- data.frame()
CHcombined_df_truth_filtered_weekday <- data.frame()
CHcombined_df_nowcast_weekday <- data.frame()
start_date <- "2021-02-01"
end_date<- "2021-02-02"
current_date <- as.Date(start_date)
end_date <- as.Date(end_date)

# While-Schleife
while (current_date <= end_date) {
  # Aufruf von epinow_einfach für das aktuelle Datum
  print(current_date)
  nowcast_try_week <- epinow_einfach(
    df = df,
    latest_date = current_date,
    locationfilter = locationfilter,
    removed_days = removed_days,
    included_days = included_days,
    maxed_delay = maxed_delay
  )

  ergebnis_t <- nowcast_try_week[["ergebnis"]]
  wis <- nowcast_try_week[["wis"]]
  final_summary_t <- nowcast_try_week[["final_summary"]]
  df_truth_t <- nowcast_try_week[["df_truth_filtered"]]
  df_nowcast_t <- nowcast_try_week[["df_nowcast"]]

  CHcombined_ergebnis_weekday <- rbind(CHcombined_ergebnis_weekday, ergebnis_t)
  CHcombined_wis_weekday <- rbind(CHcombined_wis_weekday, wis)
  CHcombined_final_summary_weekday <- rbind(CHcombined_final_summary_weekday, final_summary_t)
  CHcombined_df_truth_filtered_weekday <- rbind(CHcombined_df_truth_filtered_weekday, df_truth_t)
  CHcombined_df_nowcast_weekday <- rbind(CHcombined_df_nowcast_weekday, df_nowcast_t)
  

  current_date <- current_date + 1
}


######### später speichern der Ergebnisse
#write.csv(GEcombined_ergebnis_weekday,"Epinow/CHergebnisstartFeb.csv",row.names=FALSE)
#write.csv(GEcombined_wis_weekday,"Epinow/CHwisstartFeb.csv",row.names=FALSE)
#write.csv(GEcombined_final_summary_weekday,"Epinow/CHfinalsummarystartFeb.csv",row.names=FALSE)
#write.csv(GEcombined_df_truth_filtered_weekday,"Epinow/CHtruthstartFeb.csv",row.names=FALSE)
#write.csv(GEcombined_df_nowcast_weekday,"Epinow/CHnowcaststartFeb.csv",row.names=FALSE)


