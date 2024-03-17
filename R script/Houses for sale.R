library(readxl)
library(tidyverse)
library(fable)
library(tsibble)

Houses_sold_raw_1 <- read_excel("Data/House Market by area - 2004-2007 Regional monthly.xlsx", 
                                skip = 2)

Houses_sold_raw_2 <- read_excel("Data/House Market by area - 2008-2024 Regional monthly.xlsx", 
                                skip = 2)

Houses_for_sale <- Houses_sold_raw_1 |> 
  select(-...1,-...2) |> 
  rename(Kommune = ...3) |> 
  pivot_longer(cols = "2004M01":"2007M12",
               names_to = "Date",
               values_to = "Houses_for_sale") |> 
  bind_rows(Houses_sold_raw_2 |> 
              select(-...1,-...2) |> 
              rename(Kommune = ...3) |> 
              pivot_longer(cols = "2008M01":"2024M02",
                           names_to = "Date",
                           values_to = "Houses_for_sale")) |> 
  mutate(Date = yearquarter(Date)) |> 
  group_by(Kommune, Date) |> 
  summarise(Houses_for_sale = mean(Houses_for_sale)) |> 
  mutate(diff_Houses_for_sale = Houses_for_sale - lag(Houses_for_sale)) |> 
  as_tsibble(key = "Kommune", index = Date)
  
Houses_for_sale |> 
  filter(Kommune == "Aabenraa") |> 
  autoplot(diff_Houses_for_sale)


## Looks kind of like white noise after differentiated
