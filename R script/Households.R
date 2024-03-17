library(readxl)
library(fable)
library(tsibble)
library(tidyverse)

Households_raw <- read_excel("Data/Households per region Annualy.xlsx", 
                                            skip = 2)

Household <- Households_raw |> 
  rename(Year = ...1) |> 
  mutate(Year = as.integer(Year)) |> 
  pivot_longer("Copenhagen":"Aalborg",
               names_to = "Kommune",
               values_to = "Households") |> 
  group_by(Kommune) |> 
  mutate(log_Households = log(Households),
    diff_log_Households = log_Households - lag(log_Households,1)) |> 
  as_tsibble(key = "Kommune", index = "Year")

Household |> 
  filter(Kommune == "Copenhagen") |> 
  autoplot(diff_log_Households)

