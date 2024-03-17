library(readxl)

Population_raw <- read_excel("Data/Population By region quarterly.xlsx", 
                                                           skip = 2)

Population <- Population_raw |> 
  select(-...1,-...2) |> 
  rename(Kommune = ...3,
         Date = ...4,
         Population = Total) |> 
  fill(Kommune) |> 
  mutate(Date = yearquarter(Date),
         diff_Population = Population - lag(Population)) |> 
  as_tsibble(key = "Kommune", index = Date)

Population |> 
  filter(Kommune == "Copenhagen") |> 
  autoplot(diff_Population)
