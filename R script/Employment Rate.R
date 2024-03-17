library(readxl)
library(tidyverse)
library(fable)
library(tsibble)

Employmet_Rate_raw <- read_excel("Data/Employmet Rate National Quarterly.xls", 
                                                skip = 10)

Employmet_Rate <- Employmet_Rate_raw |> 
  rename(Date = observation_date,
         Employment_Rate = LREM64TTDKQ156S) |> 
  mutate(Date = yearquarter(Date),
         diff_Employment_Rate = Employment_Rate - lag(Employment_Rate)) |> 
  as_tsibble(index = Date)

Employmet_Rate |> 
  autoplot(diff_Employment_Rate)