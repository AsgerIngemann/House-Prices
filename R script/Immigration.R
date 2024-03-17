
Invandring_raw <- read_excel("Data/Invandring Regional Quarter.xlsx", 
                                          skip = 2)
Indvandring <- Invandring_raw |> 
  filter(Copenhagen != is.na(Copenhagen)) |> 
  mutate(Date = seq(as.Date("2007-04-01"), as.Date("2023-12-01"), by = "1 quarter"),
         Date = yearquarter(Date)) |> 
  pivot_longer("Copenhagen":"Aalborg",
               names_to = "Kommune",
               values_to = "Immigration") |> 
  select(-...1) |> 
  group_by(Kommune) |> 
  mutate(diff_Immigration = Immigration - lag(Immigration)) |> 
  as_tsibble(key = "Kommune", index = "Date")

Indvandring |> 
  filter(Kommune == "Copenhagen") |> 
  autoplot(diff_Immigration)

## Still seems to be some seasonality in our data         