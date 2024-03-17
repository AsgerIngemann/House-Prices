

Vacancy_raw <- read_excel("Data/Vacant dwellings Regional Yearly.xlsx", 
                                               skip = 2)

Vacant_Houses <- Vacancy_raw |> 
  select(-...1) |> 
  rename(Kommune = ...2) |> 
  filter(Kommune != is.na(Kommune)) |> 
  pivot_longer(cols = "2010":"2023",
               names_to = "Year",
               values_to = "Vacant_Houses") |> 
  mutate(Year = as.integer(Year),
         diff_Vacant_Houses = Vacant_Houses - lag(Vacant_Houses)) |> 
  as_tsibble(key = "Kommune", index = "Year")

## I do not think it is good enough data..