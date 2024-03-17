library(readxl)
library(tidyverse)
library(mapDK)
library(tsibble)
library(fable)

Grundskylds_raw <- read_excel("Data/Grundskylds_Promille Regional Yearly.xlsx")

column_names <- as.character(1993:2023)

Grundskyld <- Grundskylds_raw |> 
  slice(-c(1, 2,101:113)) |> 
  mutate(across(all_of(column_names), as.double)) |> 
  pivot_longer(cols = column_names,
               names_to = "Year",
               values_to = "Grundskyld") |> 
  select(-'2024') |> 
  left_join(plotDK::municipality_info, by = c("Kom.nr" = "municipality_numbers")) |> 
  rename(Kommune = municipality_names) |> 
  select(-...1) |> 
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(key = "Kommune", index = Year)

Grundskyld |> 
  filter(Kommune == "aalborg") |> 
  autoplot(Grundskyld)

## Not sure what to do about this one  