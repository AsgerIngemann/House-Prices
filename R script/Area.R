

Area_raw <- read_excel("Data/Area Region Yearly .xlsx", 
                                  skip = 2)

Area <- Area_raw |> 
  rename(Kommune = ...1) |> 
  filter(Kommune != is.na(Kommune)) |> 
  mutate(across("2007":"2024", as.integer)) |> 
  pivot_longer(cols = "2007":"2024",
               names_to = "Year",
               values_to = "Area") |> 
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(key = "Kommune", index = "Year")

  
Area |> 
  filter(Kommune == "Copenhagen") |> 
  autoplot()


## Not sure how useful this is for predictig.