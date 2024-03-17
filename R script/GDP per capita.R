library(readxl)

GDP_raw <- read_excel("Data/GDP pr Capita Yearly National.xlsx", 
                                  skip = 2)
GDP <- GDP_raw |> 
  select(-...1) |> 
  rename(Year = ...2,
         GDP = 'Pr. capita. Current prices, (1000 DKK.)') |> 
  mutate(Year = as.integer(Year)) |> 
  left_join(CPI |> 
              as_tibble() |> 
              select(Year, Index) |> 
              group_by(Year) |> 
              summarise(Index = mean(Index)), by = "Year") |> 
  filter(Year != is.na(Year)) |> 
  mutate(diff_GDP = GDP - lag(GDP)) |> 
  as_tsibble(index = Year)

GDP |> 
  autoplot(diff_GDP)

