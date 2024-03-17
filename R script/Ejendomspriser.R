library(readxl)
library(tidyverse)
library(fable)
library(tsibble)
library(mapDK)
library(plotDK)

CPI <- read_excel("Data/Consumer price index (2015 = 100) Monthly National, General.xlsx", skip = 2) |> 
  rename(Index = "Consumer price index",
         Date = ...1) |> 
  filter(Index != is.na(Index)) |> 
  mutate(Date = seq(as.Date("1980-01-01"), as.Date("2024-01-01"), by = "1 month")) |> 
  mutate(Date = yearquarter(Date)) |> 
  group_by(Date) |> 
  summarise(Index = mean(Index)) |> 
  as_tsibble(key = "Index", index = "Date") |> 
  mutate(Index = round(Index, 2),
         Year = as.integer(substr(Date, start = 1, stop = 4)))

Ejendomspriser <- read_excel("Data/Ejendomspriser - Quarter 1992 - 2023 - Kommuner .xlsx", skip = 2) |> 
  select(-...1, -...2) |> 
  rename(Kommune = ...3) |> 
  mutate(across(-Kommune, as.numeric)) |> 
  pivot_longer(cols = "1992K1":"2023K3",
               names_to = "Date",
               values_to = "House_Price") |> 
  filter(!Kommune %in% c("Bornholm","Ærø","Læsø","Fanø","Samsø")) |> 
  mutate(Date = str_replace(Date, "K", "Q")) |> 
  mutate(Date = yearquarter(Date)) |> 
  group_by(Kommune) |> 
  as_tsibble(key = "Kommune", index = "Date") |> 
  left_join(CPI, by = "Date") |> 
  filter(Index != is.na(Index)) |> 
  mutate(House_Price_CPI = (House_Price / Index)*100,
         House_Price_CPI = round(House_Price_CPI,2),
         log_House_Price_CPI = log(House_Price_CPI)) |> 
  as_tibble() |> 
  group_by(Kommune) |> 
  mutate(Growth_rate = as.numeric((House_Price_CPI - lag(House_Price_CPI)) / lag(House_Price_CPI))) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Date")

Avearge_House_Prices <- Ejendomspriser |> 
  as_tibble() |> 
  select(Date, log_House_Price_CPI) |> 
  group_by(Date) |> 
  summarise(log_House_Price_CPI = mean(log_House_Price_CPI)) |> 
  as_tsibble(index = "Date") |> 
  autoplot(exp(log_House_Price_CPI), color = "blue") +
  theme_bw() +
  labs(
    y = "",
    x = "",
    title = "Average real house price in Denmark 2001 - 2023"
  )

Training_Set <- Ejendomspriser[Ejendomspriser$Date < yearquarter("2020 Q1"),]
Testing_Set <- Ejendomspriser[Ejendomspriser$Date > yearquarter("2020 Q1"),]

fit <- Training_Set |> 
  filter(Growth_rate != is.na(Growth_rate)) |> 
  model(
    Ets = ETS(Growth_rate),
    Arima = ARIMA(Growth_rate),
    Snaive = SNAIVE(Growth_rate),
    AR_1 = AR(Growth_rate ~ order(1)),
    AR_2 = AR(Growth_rate ~ order(2)),
    AR_3 = AR(Growth_rate ~ order(3))
  )

forecast <- fit |> 
  forecast(h = "1 year")

forecast |> 
  accuracy(Testing_Set) |> 
  group_by(.model) |> 
  summarise(RMSE = mean(RMSE)) |> 
  arrange(RMSE)

## The lowest value is ETS?

fit |> 
  forecast(h = "1 year") |>
  filter(Date == yearquarter("2023 Q4")) |> 
  mapDK(value = ".mean", id = "Kommune", 
        guide.label = "Predicted Growth %",
        map.title = "Predicted Growth in House Prices") +
  scale_fill_continuous(low = "tomato3", high = "springgreen3")

fit |> 
  forecast(h = "1 year") |>
  filter(Kommune == "Frederikshavn") |> 
  autoplot(Ejendomspriser |> 
             filter(Kommune == "Frederikshavn"))

Ejendomspriser |> 
  filter(Kommune == "Frederikshavn") |> 
  autoplot(Growth_rate)
                         


