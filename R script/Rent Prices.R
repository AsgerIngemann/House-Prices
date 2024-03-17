
Rent_Prices_raw <- read_excel("Data/Rent Prices National Quarterly.xlsx", 
                                             skip = 5)

Rent_Prices <- Rent_Prices_raw |> 
  select(-...2,-...4) |> 
  rename(Rent_Price = ...3) |> 
  filter(Rent_Price != is.na(Rent_Price)) |> 
  mutate(Date = seq(as.Date("1967-01-01"), as.Date("2024-01-01"), by = "1 quarter")) |> 
  select(Rent_Price, Date) |> 
  mutate(Date = yearquarter(Date),
         diff_Rent_Price = Rent_Price - lag(Rent_Price)) |> 
  as_tsibble(index = Date)

Rent_Prices |> 
  autoplot(diff_Rent_Price)

## Pretty happy with that.