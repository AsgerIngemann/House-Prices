
CCI_raw <- read_excel("Data/Construction cost index National Quarterly.xlsx", 
                                                         skip = 2)

CCI <- CCI_raw |> 
  select(-...1, -...2, -...3, -...4) |> 
  filter(`2003Q1` != is.na(`2003Q1`)) |> 
  pivot_longer(cols = "2003Q1":"2023Q4",
               names_to = "Date",
               values_to = "CCI") |> 
  mutate(Date = yearquarter(Date),
         diff_CCI = CCI - lag(CCI)) |>
  as_tsibble(index = "Date")

CCI |> 
  autoplot(diff_CCI)


## Looks a bit like white noise some outlieers in 2022 (Corona maybe)