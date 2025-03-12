library(dplyr)
library(Matrix)
library(scales)
library(progress)
library(pbapply)
library(tidyverse)
library(runner)
library(RColorBrewer)
library(epinowcast)
meandelay<-12
maxdelay<-36
nowcast_start <- "2021-02-01"
locationfilter <- "CH"
while_end_date <- "2021-02-02"
startdate <-"2020-11-05"
daysback <- 36

source("allinone.R")
df<- read.csv("reporting_hospitalizations.csv")

nowcast_try <- kassteelen_einfach(df,meandelay,maxdelay,nowcast_start,while_end_date,startdate,daysback,locationfilter)
ergebnis_t <-nowcast_try[["ergebnis"]]
wis <-nowcast_try[["wis"]]
final_summary_t <-nowcast_try[["final_summary"]]
df_truth_t <-nowcast_try[["df_truth_filtered"]]
df_nowcast_t <-nowcast_try[["df_nowcast"]]

meanew <- nowcast_try
meanewgebnis_t <- ergebnis_t
meanewwis <-wis
meanewfinalsummary_t <- final_summary_t
meanewdf_truth_t <- df_truth_t
meanewdfnowcast_t <-df_nowcast_t

#write.csv(meanewgebnis_t,"Kassteele/KergebnisstartFeb.csv",row.names=FALSE)
#write.csv(meanewwis,"Kassteele/KwisstartFeb.csv",row.names=FALSE)
#write.csv(meanewfinalsummary_t,"Kassteele/KfinalsummarystartFeb.csv",row.names=FALSE)
#write.csv(meanewdf_truth_t,"Kassteele/KtruthstartFeb.csv",row.names=FALSE)
#write.csv(meanewdfnowcast_t,"Kassteele/KnowcaststartFeb.csv",row.names=FALSE)

