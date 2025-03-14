########## this file is not necessary if no own nowcasts are used ##########


library(dplyr)         
########## read parts of the Nowcasts and put them together ##########
setwd("Epinow/GE/summary")
csv_files <- list.files(pattern = "\\.csv$")
data_list <- lapply(csv_files, read.csv)
combined_data <- do.call(rbind, data_list)

write.csv(combined_data,"summaryGE.csv",row.names=FALSE)
setwd("..")
setwd("..")
setwd("..")
########## Kassteele fix quantile names for nowcasts ##########
df_summary<- read.csv("Kassteele/GE/KasGEnowcast2_7.csv")
df <-df_summary
df <- df %>%
  rename(q5 = lwr)
df <- df %>%
  rename(q95 = upr)
df <- df %>%
  rename(median = med) 
df <- df %>% 
  mutate(forecast_date = as.Date(forecast_date)) %>% 
  filter(format(forecast_date, "%m") != "07")
write.csv(df,"Kassteele/GEnowcast.csv",row.names=FALSE)

########## Epinowcast fix columnnames only for nowcasts not finaly summary ##########
#combined_data <- combined_data %>%
#  rename(forecast_date = report_date)
#combined_data <- combined_data %>%
#  rename(target_end_date = reference_date)
