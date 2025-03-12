########## analyse data ##########
########## plots etc can be changed to case/death data
########## cases daa did not work well 
########## was also checked
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
Sys.setlocale("LC_TIME", "en_US") 


######max reporting delay
df<- read.csv("reporting_hospitalizations.csv")

df <- df%>%
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
  mutate(date_case = as.Date(date_case))
########change dates for dataexploration
df_filtered <- df %>%
  filter(date_case >= as.Date("2021-02-01") & date_case < as.Date("2021-07-01"))
df_filtered <- df_filtered %>%
  filter(geoRegion == "CH")
#plot histogram
ggplot(df_filtered, aes(x = factor(delay))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Häufigkeit der Delay-Werte",
       x = "Delay (unique Werte)",
       y = "Häufigkeit") +
  scale_x_discrete(breaks = c("10", "20","35","36")) +  
  theme_minimal()
##### check quantiles
quartiles_2021 <- quantile(df_filtered$delay, probs = c(0.76,0.80,0.83, 0.99))

mean_delay_2021 <- df_filtered %>%
  filter(format(date_case, "%Y") == "2021") %>%
  summarize(mean_delay = mean(delay, na.rm = TRUE)) %>%
  pull(mean_delay)

#############plot data
df<- read.csv("reporting_hospitalizations.csv")
df <- df %>%
  filter(date_hospitalization >= as.Date("2021-02-01") & date_hospitalization < as.Date("2021-07-01"))
df$date_hospitalization <- as.Date(df$date_hospitalization)
df <- df %>%
  group_by(date_hospitalization) %>%
  mutate(MaxTotal = max(total, na.rm = TRUE)) %>%
  ungroup()  # Optional: Gruppierung aufheben
df_plot <- df %>%
  distinct(date_hospitalization, MaxTotal)
ggplot(df_plot, aes(x = date_hospitalization, y = MaxTotal)) +
  geom_line(color = "skyblue3") +
  labs(
       x = "Date",
       y = "Hospitalizations") +
  theme_minimal()

#################### Alle unique geoRegions extrahieren
df<- read.csv("reporting_hospitalizations.csv")

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
geoRegions <- unique(df$geoRegion)



specific_region <- "CH"
start_date <- as.Date("2022-02-01")
end_date <- as.Date("2022-04-30")

df_filtered <- df %>%
  filter(geoRegion == specific_region,
         date_case >= start_date,
         date_case <= end_date)

quantiles <- quantile(df_filtered$delay, probs = c(0.75, 0.80, 0.85, 0.90, 0.95,0.99), na.rm = TRUE)

quantiles

mean_delay <- mean(df_filtered$delay, na.rm = TRUE)

results <- list()

for (region in geoRegions) {
  df_region <- df %>% filter(geoRegion == region)
    quartiles_all <- quantile(df_region$delay, probs = c(0.75, 0.8, 0.85, 0.9, 0.95), na.rm = TRUE)
    years <- unique(format(df_region$date_case, "%Y"))
  for (year in years) {
    df_year <- df_region %>% filter(format(date_case, "%Y") == year)
    quartiles_year <- quantile(df_year$delay, probs = c(0.75, 0.8, 0.85, 0.9, 0.95), na.rm = TRUE)
        results[[paste(region, year, sep = "_")]] <- data.frame(
      geoRegion = region,
      year = year,
      Q75 = quartiles_year[1],
      Q80 = quartiles_year[2],
      Q85 = quartiles_year[3],
      Q90 = quartiles_year[4],
      Q95 = quartiles_year[5]
    )
  }
}
final_results <- do.call(rbind, results)

print(final_results)




#################### plot delay per name cantons ####################
df<- read.csv("reporting_hospitalizations.csv")
df$date_hospitalization <- as.Date(df$date_hospitalization)
df <- df %>%
  rename(date_case = date_hospitalization)

group_a <- c("AG", "AI", "AR")
group_b <- c("BE", "BL", "BS")
group_c <- c("CH", "CHFL")
group_f <- c("FL", "FR")
group_g <- c("GE", "GL", "GR")
group_j_l <- c("JU", "LU")
group_n_o <- c("NE", "NW", "OW")
group_s <- c("SG", "SH", "SO", "SZ")
group_t_u <- c("TG", "TI", "UR")
group_v_z <- c("VD", "VS", "ZG", "ZH")
plot_group_data <- function(df, group, group_name) {
  df_group <- df %>%
    filter(geoRegion %in% group) %>%
    group_by(geoRegion, date_case) %>%
    summarise(total_delay = sum(total, na.rm = TRUE), .groups = "drop")
  
  ggplot(df_group, aes(x = date_case, y = total_delay, color = geoRegion)) +
    geom_line() +
    labs(title = paste("Delay für Gruppe", group_name),
         x = "Datum",
         y = "Verzögerung") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

plot_a <- plot_group_data(df, group_a, "A")
plot_b <- plot_group_data(df, group_b, "B")
plot_c <- plot_group_data(df, group_c, "C")
plot_f <- plot_group_data(df, group_f, "F")
plot_g <- plot_group_data(df, group_g, "G")
plot_j_l <- plot_group_data(df, group_j_l, "J & L")
plot_n_o <- plot_group_data(df, group_n_o, "N & O")
plot_s <- plot_group_data(df, group_s, "S")
plot_t_u <- plot_group_data(df, group_t_u, "T & U")
plot_v_z <- plot_group_data(df, group_v_z, "V & Z")

print(plot_a)
print(plot_b)
print(plot_c)
print(plot_f)
print(plot_g)
print(plot_j_l)
print(plot_n_o)
print(plot_s)
print(plot_t_u)
print(plot_v_z)

###############################################################################
###############################################################################
###############################################################################
########################## look at measels ####################################
###############################################################################
###############################################################################
###############################################################################
df_measels <- read.delim("C:/Users/natal/Documents/Uni/0_WiSe202425/Statistics and Epidemics/DataAnalysis/measels.dat", header = TRUE)

df_measels <- read.delim("measels.dat", header = TRUE)
######## measels data import from
#https://github.com/kassteele/Nowcasting/blob/master/data/measles_NL_2013_2014.dat
########
df_measels_summary <- df_measels %>%
  group_by(onset.date) %>%
  summarise(hospi = n()) %>%  
  ungroup()  

head(df_measels_summary)
df_measels_summary$onset.date <- as.Date(df_measels_summary$onset.date)
ggplot(df_measels_summary, aes(x = onset.date, y = hospi)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(
       x = "Onset Date",
       y = "Hospitalisierungen (hospi)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
