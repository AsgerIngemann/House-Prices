

Permits_raw <- read_excel("Data/Residential Construction Regional Quarterly.xlsx", 
                                                          skip = 2)

Permits <- Permits_raw |> 
  select(-...1,-...2) |> 
  rename(Kommune = ...3) |> 
  filter(Kommune != is.na(Kommune)) |> 
  pivot_longer(cols = "2006Q1":"2023Q4",
               names_to = "Date",
               values_to = "Permits") |> 
  mutate(Date = yearquarter(Date),
         diff_Permits = Permits - lag(Permits)) |> 
  as_tsibble(key = "Kommune", index = "Date")

Permits |> 
  filter(Kommune == "Copenhagen") |> 
  autoplot(diff_Permits)


## Looks like some white noise after differentiated.
