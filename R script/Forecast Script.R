rm(list = ls())
setwd("~/Documents/UNI/Kandidaten/Economic Forecasting/Data")

library(readxl)
Realiserede_Ejendomspriser_Hele_Landet <- read_excel("Realiserede Ejendomspriser - Hele Landet.xlsx", 
                                                     col_types = c("text", "numeric"))
View(Realiserede_Ejendomspriser_Hele_Landet)

Year <- substring(Realiserede_Ejendomspriser_Hele_Landet$Date,1,4)
Quarter <- substring(Realiserede_Ejendomspriser_Hele_Landet$Date,5)
Quarter <- gsub("K","Q", Quarter)