
Price_to_Rent_ratio_raw <- read_excel("Data/Price to Rent ratio National Quarterly.xlsx", 
                                                     skip = 5)

Price_to_Rent_ratio <- Price_to_Rent_ratio_raw |> 
  select(-...2,-...4) |> 
  rename(PTRR = ...3) |> 
  filter(PTRR != is.na(PTRR)) |> 
  mutate(Date = seq(as.Date("1970-01-01"), as.Date("2023-08-08"), by = "1 quarter")) |> 
  select(PTRR, Date) |> 
  mutate(Date = yearquarter(Date),
         diff_PTRR = PTRR - lag(PTRR)) |> 
  as_tsibble(index = Date)

Price_to_Rent_ratio |> 
  autoplot(diff_PTRR)


## Pretty happy with that white noise.