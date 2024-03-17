library(readxl)
library(tidyverse)
library(fable)
library(tsibble)
library(mapDK)
library(plotDK)


### FULL DATA LOAD

CPI <- read_excel("Data/Consumer price index (2015 = 100) Monthly National, General.xlsx", skip = 2) |> 
  rename(Index = "Consumer price index",
         Date = ...1) |> 
  filter(Index != is.na(Index)) |> 
  mutate(Date = seq(as.Date("1980-01-01"), as.Date("2024-01-01"), by = "1 month")) |> 
  mutate(Date = yearquarter(Date)) |> 
  group_by(Date) |> 
  summarise(Index = mean(Index)) |> 
  as_tsibble(key = "Index", index = "Date") |> 
  mutate(Index = round(Index, 2),
         Year = as.integer(substr(Date, start = 1, stop = 4)))

Ejendomspriser <- read_excel("Data/Ejendomspriser - Quarter 1992 - 2023 - Kommuner .xlsx", skip = 2) |> 
  select(-...1, -...2) |> 
  rename(Kommune = ...3) |> 
  mutate(across(-Kommune, as.numeric)) |> 
  pivot_longer(cols = "1992K1":"2023K3",
               names_to = "Date",
               values_to = "House_Price") |> 
  filter(!Kommune %in% c("Bornholm","Ærø","Læsø","Fanø","Samsø")) |> 
  mutate(Date = str_replace(Date, "K", "Q")) |> 
  mutate(Date = yearquarter(Date)) |> 
  group_by(Kommune) |> 
  as_tsibble(key = "Kommune", index = "Date") |> 
  left_join(CPI, by = "Date") |> 
  filter(Index != is.na(Index)) |> 
  mutate(House_Price_CPI = (House_Price / Index)*100,
         House_Price_CPI = round(House_Price_CPI,2),
         log_House_Price_CPI = log(House_Price_CPI)) |> 
  as_tibble() |> 
  group_by(Kommune) |> 
  mutate(Growth_rate = as.numeric((House_Price_CPI - lag(House_Price_CPI)) / lag(House_Price_CPI))) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Date")

Interest_rates <- read_excel("Data/Nationalbankens rente - Udlån - Monthly National.xlsx", 
                                                               skip = 2) |> 
              select(-...1) |> 
              rename(Rate = Månedsultimo) |>
              filter(Rate != is.na(Rate)) |> 
              mutate(Date = seq(as.Date("1985-10-01"), as.Date("2024-02-01"), by = "1 month"),
                     Date = yearquarter(Date)) |>
              select(-...2) |> 
              mutate(Rate = as.double(Rate)) |> 
              filter(Rate != is.na(Rate)) |> 
              group_by(Date) |> 
              summarise(Rate = mean(Rate)) |> 
              mutate(diff_Rate = Rate - lag(Rate)) |> 
              as_tsibble(index = "Date")

Disposable_Income <-  read_excel("Data/Disposable Income by region Yearly.xlsx", 
                                 skip = 2) |> 
  select(...3, ...4, "1 Disposable income (2+30-31-32-35)") |> 
  rename(Year = ...4,
         Disp_Income = '1 Disposable income (2+30-31-32-35)',
         Kommune = ...3) |> 
  fill(Kommune) |>
  filter(Disp_Income != is.na(Disp_Income)) |>
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(index = Year, key = "Kommune") |>
  left_join(CPI |> 
              as_tibble() |> 
              select(Year, Index) |> 
              group_by(Year) |> 
              summarise(Index = mean(Index)), by = "Year") |> 
  mutate(Disp_Income_CPI = (Disp_Income / Index)*100,
         log_Disp_Income = log(Disp_Income),
         diff_log_Disp_Income = log_Disp_Income - lag(log_Disp_Income))

Consumer_Confidence <- read_excel("Data/Consumer Confidence - National Monthly.xlsx", 
                                     skip = 2) |> 
  rename(Consumer_Confidence = 'Consumer confidence indicator') |> 
  mutate(Date = seq(as.Date("1974-10-01"), as.Date("2024-02-01"), by = "1 month"),
         Date = yearquarter(Date)) |> 
  select(-...1) |> 
  group_by(Date) |> 
  mutate(Consumer_Confidence = as.integer(Consumer_Confidence)) |> 
  filter(Consumer_Confidence != is.na(Consumer_Confidence)) |> 
  summarise(Consumer_Confidence = mean(Consumer_Confidence)) |> 
  mutate(diff_Consumer_Confidence = Consumer_Confidence - lag(Consumer_Confidence)) |> 
  as_tsibble(index = "Date")

Area <- read_excel("Data/Area Region Yearly .xlsx", 
                   skip = 2) |> 
  rename(Kommune = ...1) |> 
  filter(Kommune != is.na(Kommune)) |> 
  mutate(across("2007":"2024", as.integer)) |> 
  pivot_longer(cols = "2007":"2024",
               names_to = "Year",
               values_to = "Area") |> 
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(key = "Kommune", index = "Year")

CCI <- read_excel("Data/Construction cost index National Quarterly.xlsx", 
                  skip = 2) |> 
  select(-...1, -...2, -...3, -...4) |> 
  filter(`2003Q1` != is.na(`2003Q1`)) |> 
  pivot_longer(cols = "2003Q1":"2023Q4",
               names_to = "Date",
               values_to = "CCI") |> 
  mutate(Date = yearquarter(Date),
         diff_CCI = CCI - lag(CCI)) |>
  as_tsibble(index = "Date")

Employmet_Rate <- read_excel("Data/Employmet Rate National Quarterly.xls", 
                                 skip = 10) |> 
  rename(Date = observation_date,
         Employment_Rate = LREM64TTDKQ156S) |> 
  mutate(Date = yearquarter(Date),
         diff_Employment_Rate = Employment_Rate - lag(Employment_Rate)) |> 
  as_tsibble(index = Date)

GDP <- read_excel("Data/GDP pr Capita Yearly National.xlsx", 
                      skip = 2) |> 
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

Gross_Lending <- read_excel("Data/Mortgage Banks Gross Lending - Quarterly National.xlsx", 
                            skip = 2) |> 
  select(-...1,-...2) |> 
  rename(Date = ...3,
         Gross_Lending = "All loan types") |> 
  mutate(Date = yearquarter(Date),
         diff_Gross_Lending = Gross_Lending - lag(Gross_Lending)) |> 
  as_tsibble(index = "Date")

column_names <- as.character(1993:2023)
Key_for_Regions <- read_excel("Data/Key for Regions.xlsx") |> 
  rename(Kom.nr = NUTS_KODE,
         Kommune = NUTS_TXT) |> 
  select(Kom.nr, Kommune) |> 
  distinct(Kom.nr, .keep_all = TRUE)


Grundskyld <- read_excel("Data/Grundskylds_Promille Regional Yearly.xlsx") |> 
  slice(-c(1, 2,101:113)) |> 
  mutate(across(all_of(column_names), as.double)) |> 
  pivot_longer(cols = column_names,
               names_to = "Year",
               values_to = "Grundskyld") |> 
  select(-'2024') |> 
  left_join(Key_for_Regions, by = c("Kom.nr" = "Kom.nr")) |> 
  select(-...1) |> 
  mutate(Year = as.integer(Year),
         Kommune = case_when(Kom.nr == 715 ~ "Aarhus",
                             Kom.nr != 715 ~ Kommune)) |> 
  as_tsibble(key = "Kommune", index = Year)


Household <- read_excel("Data/Households per region Annualy.xlsx", 
                             skip = 2) |> 
  rename(Year = ...1) |> 
  mutate(Year = as.integer(Year)) |> 
  pivot_longer("Copenhagen":"Aalborg",
               names_to = "Kommune",
               values_to = "Households") |> 
  group_by(Kommune) |> 
  mutate(log_Households = log(Households),
         diff_log_Households = log_Households - lag(log_Households,1)) |> 
  as_tsibble(key = "Kommune", index = "Year")

Houses_sold_raw_1 <- read_excel("Data/House Market by area - 2004-2007 Regional monthly.xlsx", 
                                skip = 2)

Houses_sold_raw_2 <- read_excel("Data/House Market by area - 2008-2024 Regional monthly.xlsx", 
                                skip = 2)

Houses_for_sale <- Houses_sold_raw_1 |> 
  select(-...1,-...2) |> 
  rename(Kommune = ...3) |> 
  pivot_longer(cols = "2004M01":"2007M12",
               names_to = "Date",
               values_to = "Houses_for_sale") |> 
  bind_rows(Houses_sold_raw_2 |> 
              select(-...1,-...2) |> 
              rename(Kommune = ...3) |> 
              pivot_longer(cols = "2008M01":"2024M02",
                           names_to = "Date",
                           values_to = "Houses_for_sale")) |> 
  mutate(Date = yearquarter(Date)) |> 
  group_by(Kommune, Date) |> 
  summarise(Houses_for_sale = mean(Houses_for_sale)) |> 
  mutate(diff_Houses_for_sale = Houses_for_sale - lag(Houses_for_sale)) |> 
  as_tsibble(key = "Kommune", index = Date)

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

Rent_Prices_raw <- read_excel("Data/Rent Prices National Quarterly.xlsx", 
                              skip = 5)

Rent_Prices <- Rent_Prices_raw |> 
  select(-...2,-...4) |> 
  rename(Rent_Price = ...3) |> 
  filter(Rent_Price != is.na(Rent_Price)) |> 
  mutate(Date = seq(as.Date("1967-01-01"), as.Date("2024-01-01"), by = "1 quarter")) |> 
  select(Rent_Price, Date) |> 
  mutate(Date = yearquarter(Date),
         diff_Rent_Price = Rent_Price - lag(Rent_Price)) |> 
  as_tsibble(index = Date)

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


Ejendomspriser <- Ejendomspriser |> 
  left_join(Key_for_Regions, by = "Kommune") |> 
  mutate(Kom.nr = case_when(Kommune == "Århus" ~ 751,
                            Kommune != "Århus" ~ Kom.nr)) |> 
  left_join(Interest_rates, by = "Date") |> 
  left_join(Disposable_Income, by = c("Kommune", "Year")) |> 
  left_join(Consumer_Confidence, by ="Date") |> 
  left_join(Area, by = c("Kommune", "Year")) |> 
  left_join(CCI, by = "Date") |> 
  left_join(Employmet_Rate, by = "Date") |> 
  left_join(GDP, by = "Year") |> 
  left_join(Gross_Lending, by = "Date") |>
  left_join(Grundskyld, by = c("Kommune", "Year")) |> 
  left_join(Household, by = c("Kommune", "Year")) |> 
  left_join(Houses_for_sale, by = c("Kommune", "Date")) |>
  left_join(Indvandring, by = c("Kommune", "Date")) |>
  left_join(Permits, by = c("Kommune", "Date")) |>
  left_join(Population, by = c("Kommune", "Date")) |>
  left_join(Price_to_Income_Ratio, by = "Date") |>
  left_join(Price_to_Rent_ratio, by = "Date") |>
  left_join(Rent_Prices, by = "Date") |>
  left_join(Vacant_Houses, by = c("Kommune", "Year")) |> 
  select(-Index.y, -Index.x, -Kom.nr.y)
  
##### Test / Train data split

Ejendomspriser_train <- Ejendomspriser |> filter(year(Date) <= 2018)
Ejendomspriser_test <- Ejendomspriser |> filter(year(Date) >= 2018)

#### Initial models
  
Ejendomspriser_fit <- Ejendomspriser_train |>
  model(
    Mean = MEAN(Growth_rate),
    Naive = NAIVE(Growth_rate),
    Seasonal_naive = SNAIVE(Growth_rate),
    Drift = RW(Growth_rate ~ drift())
  )

Ejendomspriser_fc <- Ejendomspriser_fit |>
  forecast(h = 20)

Ejendomspriser_fc |> 
  left_join(Ejendomspriser_test |> select(Growth_rate), by = c("Kommune", "Date")) |> 
  mutate(RSE = (Growth_rate.y - .mean)^2) |> 
  as_tibble() |> 
  group_by(Kommune, .model) |> 
  summarise(RMSE = mean(RSE, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(Kommune) |> 
  filter(RMSE == min(RMSE)) |> 
  ungroup() |> 
  count(.model, name = "Frequency")

### Clearly it seems that Mean is the best simple model for predicting.

Ejendomspriser_fit <- Ejendomspriser_train |> 
  filter(Growth_rate != is.na(Growth_rate)) |>
  model(
    Mean = MEAN(Growth_rate),
    AR = AR(Growth_rate),
    ARIMA = ARIMA(Growth_rate),
    TSLM(Growth_rate ~ trend() + fourier(K = 2)), 
    ETS = ETS(Growth_rate)
  )

Ejendomspriser_fc <- Ejendomspriser_fit |>
  forecast(h = 20)

Ejendomspriser_fc |> 
  left_join(Ejendomspriser_test |> select(Growth_rate), by = c("Kommune", "Date")) |> 
  mutate(RSE = (Growth_rate.y - .mean)^2) |> 
  as_tibble() |> 
  group_by(Kommune, .model) |> 
  summarise(RMSE = mean(RSE, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(Kommune) |> 
  filter(RMSE == min(RMSE)) |> 
  ungroup() |> 
  count(.model, name = "Frequency")

### It seems that TSLM is the prefered model AR and ARIMA model followed close by.

Ejendomspriser_fit <- Ejendomspriser_train |> 
  filter(Growth_rate != is.na(Growth_rate)) |>
  model(
    ARIMA = ARIMA(Growth_rate ~ season()),
    TSLM(Growth_rate ~ season() + fourier(K = 2))
  )

Ejendomspriser_fc <- Ejendomspriser_fit |>
  forecast(h = 20)

Ejendomspriser_fc |> 
  left_join(Ejendomspriser_test |> select(Growth_rate), by = c("Kommune", "Date")) |> 
  mutate(RSE = (Growth_rate.y - .mean)^2) |> 
  as_tibble() |X> 
  group_by(Kommune, .model) |> 
  summarise(RMSE = mean(RSE, na.rm = TRUE)) |> 
  ungroup() |> 
  group_by(Kommune) |> 
  filter(RMSE == min(RMSE)) |> 
  ungroup() |> 
  count(.model, name = "Frequency")

### Clearly there is some kind of trend one needs to take care of.

Ejendomspriser_fc |> 
  filter(Kommune == "København") |> 
  autoplot(Ejendomspriser)

Ejendomspriser |> 
  filter(Kommune == "København") |> 
  ggplot(aes(x = Date, y = Growth_rate)) +
  geom_line() +
  theme_minimal()

