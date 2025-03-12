########## plots for Seminararbeit ##########
source("prepfiles/allinone.R")
source("prepfiles/plotfunctions.R")
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
Sys.setlocale("LC_TIME", "en_US") 

########## reporting_hospitalizations must be uploaded from ##########
## https://github.com/adrian-lison/covid-reporting-switzerland/tree/main/data ##
######### and be stored in the folder ############
df_original<- read.csv("reporting_hospitalizations.csv")


########## prepare summary for WIS and empirical coverage plot overall ##########
########## files must be choosen ##########
month <-03
monthfiltern <- FALSE
ECHsummary <- read.csv("Epinow/CH/summaryCH.csv")
ECHsummary <- ECHsummary %>%
  mutate(model = gsub("Epiforecast", "Epinowcast", model))
KCHsummary <- read.csv("Kassteele/KsummaryCH.csv")
df_summary <- bind_rows(KCHsummary, ECHsummary)

plot_wis(df_summary,month,monthfiltern,"Score for Swiss data")
plot_coverage(df_summary,month,monthfiltern, "Empirical coverage for Swiss data")

########## prepare summary for WIS and empirical coverage plot monthly ##########
########## files must be choosen ##########


month <-03
monthfiltern <- FALSE
ECHsummary <- read.csv("C:/Users/natal/Documents/Uni/0_WiSe202425/Statistics and Epidemics/E/Epinow/GE/summaryGE.csv")
ECHsummary <- ECHsummary %>%
  mutate(model = gsub("Epiforecast", "Epinowcast", model))
ECHsummary <- ECHsummary %>%
  mutate(model = paste0(model, "_","0", month(ymd(forecast_date))))
KCHsummary <- read.csv("C:/Users/natal/Documents/Uni/0_WiSe202425/Statistics and Epidemics/E/Kassteele/KsummaryGE.csv")
KCHsummary <- KCHsummary %>%
  mutate(model = paste0(model, "_","0", month(ymd(forecast_date))))
df_summary <- bind_rows(KCHsummary, ECHsummary)

plot_wis_month(df_summary,month,monthfiltern,"Score for Swiss data")
plot_coverage_month(df_summary,month,monthfiltern, "Empirical coverage for Swiss data")

###### horizon nowcast plots
EGEnowcast<- read.csv("Epinow/CH/nowcastCH.csv")
KGEnowcast <- read.csv("Kassteele/KnowcastCH.csv")
df_nowcast <-KGEnowcast
##### plot for horizon #######
horizon <- -7
canton <- "CH"
df_original<- read.csv("reporting_hospitalizations.csv")
df_nowcast_0 <- filter_single_forecast(df_nowcast,horizon)
df_nowcast_0$target_end_date <- as.Date(df_nowcast_0$target_end_date)

df_truth_max<- truth_maximal(df_original,canton)
df_truth_max_f <-df_truth_max %>% filter(date >= "2021-02-01")
df_truth_max_f <-df_truth_max_f %>% filter(date < "2021-07-01")
df_truth_max_f$date <- as.Date(df_truth_max_f$date)

df_original <- df_original %>%
  rename(forecast_date = date_report)
df_original <- df_original %>%
  rename(target_end_date = date_hospitalization)
df_original_horizon <- filter_single_forecast(df_original,horizon)
df_original_horizon <- df_original_horizon %>% filter(geoRegion == canton)

df_original_horizon <-df_original_horizon %>% filter(forecast_date >= "2021-02-01")
df_original_horizon <-df_original_horizon %>% filter(forecast_date < "2021-07-01")
df_original_horizon$forecast_date <- as.Date(df_original_horizon$forecast_date)
plot_nowcast_for_one_horizon(df_original_horizon,df_nowcast_0,df_truth_max_f)

####### plot comparison real time and delayed reporting
df_original<- read.csv("reporting_hospitalizations.csv")

df_original_0 <- filter_single_forecast_rename(df_original,0)
df_original_0 <- df_original_0 %>% filter(geoRegion == "CH")
df_original_0$forecast_date<-as.Date(df_original_0$forecast_date)
df_original_0 <-df_original_0 %>% filter(forecast_date >= "2021-02-01")
df_original_0 <-df_original_0 %>% filter(forecast_date < "2021-06-30")
df_truth_max<- truth_maximal(df_original,"CH")
df_truht_max_f <-df_truth_max %>% filter(date >= "2021-02-01")
df_truht_max_f <-df_truht_max_f %>% filter(date < "2021-07-01")
ggplot() +
  geom_line(data = df_truht_max_f, aes(x = date, y = truth, group = 1, color = "cumulated reporting")) +
  geom_line(data = df_original_0, aes(x = forecast_date, y = total, group = 1,color = "real time reporting")) +
  
  labs(
    x = "Date",
    y = "Hospitalizations") +
  scale_color_manual(
    values = c("cumulated reporting" = "skyblue3", "real time reporting" = "darkred"),
    name = "Line Type"
  ) +
  theme_minimal()

######### basic nowcast ################
df_original<- read.csv("reporting_hospitalizations.csv")

all_combinations <- df_original %>%
  select(date_hospitalization) %>%
  distinct() %>%
  mutate(date_report = date_hospitalization, geoRegion = "CH", total = 0)
# fill all dates
df_original <- df_original %>%
  bind_rows(anti_join(all_combinations, df_original, by = c("date_hospitalization", "date_report", "geoRegion")))


df_original <-df_original %>% filter(geoRegion == "CH")
df_Feb <-df_original %>% filter(date_hospitalization >= "2021-02-01")
df_Feb<-df_Feb %>% filter(date_report < "2021-03-01")
df_Feb<-truth_maximal(df_Feb,"CH")
df_Mar <-df_original %>% filter(date_hospitalization >= "2021-02-01")
df_Mar<-df_Mar %>% filter(date_report < "2021-04-01")
df_Mar<-truth_maximal(df_Mar,"CH")
df_Apr <-df_original %>% filter(date_hospitalization >= "2021-02-01")
df_Apr<-df_Apr %>% filter(date_report < "2021-05-01")
df_Apr<-truth_maximal(df_Apr,"CH")
df_combined <- bind_rows(
  df_Feb %>% mutate(Month = "February"),
  df_Mar %>% mutate(Month = "March"),
  df_Apr %>% mutate(Month = "April")
)
df_combined$Month <- factor(df_combined$Month, levels = c("February", "March", "April"))


ggplot(df_combined, aes(x = date, y = truth, color = Month)) +
  geom_line(size = 0.8) +  
  labs(
    x = "Date",
    y = "Reported Hospitalizations",
    color = "Month") +
  theme_minimal() +
  scale_color_manual(values = c("February" = "#1F77B4",   
                                "March" = "#3C96C4",     
                                "April" = "#75B9D9")) +  
  geom_vline(xintercept = as.Date(c("2021-02-28", "2021-03-31", "2021-04-30")), 
             linetype = "dashed", color = "black", size = 0.5)
theme(legend.position = "top")  



