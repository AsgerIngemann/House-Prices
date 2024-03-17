Gross_Lending_raw <- read_excel("Data/Mortgage Banks Gross Lending - Quarterly National.xlsx", 
                                                              skip = 2)
Gross_Lending <- Gross_Lending_raw |> 
  select(-...1,-...2) |> 
  rename(Date = ...3,
         Gross_Lending = "All loan types") |> 
  mutate(Date = yearquarter(Date),
         diff_Gross_Lending = Gross_Lending - lag(Gross_Lending)) |> 
  as_tsibble(index = "Date")

Gross_Lending |> 
  autoplot(diff_Gross_Lending)

## Looks like white noise