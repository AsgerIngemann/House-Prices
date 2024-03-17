
### Economic Variables

library(readxl)
library(tidyverse)
library(fable)
library(tsibble)

Disposable_Income_raw <- read_excel("Data/Disposable Income by region Yearly.xlsx", 
                                                 skip = 2)
Disposable_Income <- Disposable_Income_raw |> 
  select(...3, ...4, "1 Disposable income (2+30-31-32-35)") |> 
  rename(Year = ...4,
         Disp_Income = '1 Disposable income (2+30-31-32-35)',
         Kommune = ...3) |> 
  fill(Kommune) |>
  filter(Disp_Income != is.na(Disp_Income)) |>
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(index = Year, key = "Kommune") |>
  left_join(CPI |> 
              as_tibble() |> 
              select(Year, Index) |> 
              group_by(Year) |> 
              summarise(Index = mean(Index)), by = "Year") |> 
  mutate(Disp_Income_CPI = (Disp_Income / Index)*100,
         log_Disp_Income = log(Disp_Income),
         diff_log_Disp_Income = log_Disp_Income - lag(log_Disp_Income))

Disposable_Income |> 
  filter(Kommune == "Aabenraa") |>
  autoplot(diff_log_Disp_Income)



