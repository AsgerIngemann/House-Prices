library(readxl)

Price_to_Income_Ratio_raw <- read_excel("Data/Price to Income Ratio - National Quarterly.xlsx", 
                                                       skip = 5)

Price_to_Income_Ratio <- Price_to_Income_Ratio_raw |> 
  select(...3) |> 
  rename(PTIR = ...3) |> 
  filter(PTIR != is.na(PTIR)) |> 
  mutate(Date = seq(as.Date("1981-01-01"), as.Date("2023-09-01"), by = "1 quarter"),
         Date = yearquarter(Date),
         diff_PTIR = PTIR - lag(PTIR)) |> 
  as_tsibble(index = Date)

Price_to_Income_Ratio |> 
  autoplot(diff_PTIR)
         