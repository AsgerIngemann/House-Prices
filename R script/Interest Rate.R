
Interest_rates_raw <- read_excel("Data/Nationalbankens rente - Udlån - Monthly National.xlsx", 
                                                            skip = 2)

Interest_rates <- Interest_rates_raw |> 
  select(-...1) |> 
  rename(Rate = Månedsultimo) |>
  filter(Rate != is.na(Rate)) |> 
  mutate(Date = seq(as.Date("1985-10-01"), as.Date("2024-02-01"), by = "1 month"),
         Date = yearquarter(Date)) |>
  select(-...2) |> 
  mutate(Rate = as.double(Rate)) |> 
  filter(Rate != is.na(Rate)) |> 
  group_by(Date) |> 
  summarise(Rate = mean(Rate)) |> 
  mutate(diff_Rate = Rate - lag(Rate)) |> 
  as_tsibble(index = "Date")
  
Interest_rates |> 
  autoplot(diff_Rate)

## Looks like white noise