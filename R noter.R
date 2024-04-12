data_availability <- Ejendomspriser |> 
  as.tibble() |> 
  group_by(Kommune) |> 
  gather(key = "variable", value = "value", c(-Date, -Kommune)) %>%
  group_by(Kommune, Date, variable) %>%
  summarise(has_data = !any(is.na(value)))

availability_wide <- data_availability %>%
  pivot_wider(names_from = variable, values_from = has_data)

availability_long <- availability_wide |> 
  filter(Kommune == "København") |> 
  pivot_longer(cols = -c(Date, Kommune), names_to = "variable", values_to = "availability")

availability_long %>%
  filter(!grepl("^log|^diff", variable)) %>%
  ggplot(aes(x = Date, y = variable)) +
  geom_tile(aes(fill = availability), width = .9, height = .9) +  # Use tile for background
  geom_line(aes(group = variable, color = availability), size = 2) +  # Add lines
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey")) +
  scale_color_manual(values = c("TRUE" = "darkblue", "FALSE" = "darkgrey")) +  # Optional: Adjust line colors to match fill
  labs(title = "Data Availability Over Time by Municipality",
       x = "Date",
       y = "Variable",
       fill = "Available",
       color = "Available") +  # Add color legend title if needed
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        strip.text.x = element_text(angle = 0),
        panel.spacing = unit(2, "lines"),
        strip.background = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank(),  # Remove major vertical grid lines
        panel.grid.minor.x = element_blank())  # Remove minor vertical grid lines


Density_raw <- read_excel("Data/Befolknings Tæthed by Region 1993 - 2023.xlsx",
                          skip = 2)

Vacant_Houses |> select(Kommune == "Copenhagen")

Density <- Density_raw |> 
  select(-...1, -"2024") |> 
  pivot_longer(, cols = "1993":"2023",
               names_to = "Year",
               values_to = "Density") |> 
  filter(!is.na(Kom.nr)) |>
  mutate(Kom.nr = as.double(Kom.nr),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")

Andel_Ejerboliger_raw <- read_excel("Data/Andel Ejerboliger Region Yearly 1993-2024.xlsx", 
              skip = 2)

Andel_Ejerboliger_raw |> 
  mutate('2010' = as.double('2010')) |> 
  select(-...1, -"2024", -"2022", -"2023") |> 
  pivot_longer(, cols = "1993":"2021",
               names_to = "Year",
               values_to = "Andel_Ejerboliger") |> 
  filter(!is.na(Kom.nr)) |>
  mutate(Kom.nr = as.double(Kom.nr),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")

Anmeldte_indbrud_raw <- read_excel("Data/Anmeldte indbryd Region Yearly 1993-2024.xlsx", 
           skip = 2)

Anmeldte_indbrud_raw <- Anmeldte_indbrud_raw |> 
  select(-...1, -"2024", -"2023") |> 
  pivot_longer(, cols = "1993":"2022",
               names_to = "Year",
               values_to = "Anmeldte_indbrud") |> 
  filter(!is.na(Kom.nr)) |>
  mutate(Kom.nr = as.double(Kom.nr),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")

Anmeldte_voldsforbrydelser_raw <- read_excel("Data/Anmeldte voldsforbrydelser Region Yearly 1993-2024.xlsx", 
              skip = 2)

Anmeldte_voldsforbrydelser <- Anmeldte_voldsforbrydelser_raw |> 
  mutate(across(matches('^(199[3-9]|20[0-1][0-9]|202[0-3])$'), as.double))|> 
  select(-...1, -"2024", -"2023") |> 
  pivot_longer(, cols = "1993":"2022",
               names_to = "Year",
               values_to = "Anmeldte_voldsforbrydelser") |> 
  filter(!is.na(Kom.nr)) |>
  mutate(Kom.nr = as.double(Kom.nr),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")

Udskrivningsprocent_raw <- read_excel("Data/Udskrivningsprocent Region Yearly 1993-2024.xlsx", 
                                                          skip = 2)

Udskrivningsprocent <- Udskrivningsprocent_raw |> 
  select(-...1, -"2024") |> 
  filter(!is.na(Kom.nr)) |> 
  mutate(across(where(is.character), as.numeric)) |> 
  pivot_longer(, cols = "1993":"2023",
               names_to = "Year",
               values_to = "Udskrivningsprocent") |> 
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")

Udgift_til_dagtilbud_raw <- read_excel("Data/Udgift til dagtilbud Region Yearly 1993-2024.xlsx", 
                                                           skip = 2)

Udgift_til_dagtilbud <- Udgift_til_dagtilbud_raw |> 
  select(-...1, -"2024") |> 
  filter(!is.na(Kom.nr)) |> 
  mutate(across(where(is.character), as.numeric)) |> 
  pivot_longer(, cols = "1993":"2023",
               names_to = "Year",
               values_to = "Udgift_til_dagtilbud") |> 
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")

Socioøkonomiske_indeks_raw <- read_excel("Data/Socioøkonomiske indeks Region Yearly 1996-2024.xlsx",
                                         skip = 2)

Socioøkonomiske_indeks_raw |> 
  select(-...1, -"2024") |> 
  filter(!is.na(Kom.nr)) |> 
  mutate(across(where(is.character), as.numeric)) |> 
  pivot_longer(, cols = "1993":"2023",
               names_to = "Year",
               values_to = "Udgift_til_dagtilbud") |> 
  mutate(Year = as.integer(Year)) |> 
  as_tsibble(key = "Kom.nr", index = "Year")


Vacant_Houses <- Vacancy_raw |> 
  select(-...1) |> 
  rename(Kommune = ...2) |> 
  filter(Kommune != is.na(Kommune)) |> 
  pivot_longer(cols = "2010":"2023",
               names_to = "Year",
               values_to = "Vacant_Houses") |> 
  group_by(Kommune) |> 
  mutate(Year = as.integer(Year),
         diff_Vacant_Houses = Vacant_Houses - lag(Vacant_Houses)) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Year")

Ejendomspriser |> 
  left_join(Kommune_ID |> select(Kommune_2, Kom.nr), by = "Kom.nr") |> 
  View()

Realkreditudlån_1993_2013 <- read_excel("Data/Realkreditudlån 1993-2013 Monthly.xlsx", 
                                                skip = 2)

Realkreditudlån_1 = Realkreditudlån_1993_2013 |> 
  select(-...1, -...2, -...3) |> 
  rename("Month" = '...4',
         'Udlån' = 'Alle løbetider/uspecificeret') |> 
  filter(!is.na(Udlån)) |> 
  mutate(Date = seq(as.Date("1993-01-01"), as.Date("2013-10-01"), by = "1 month"),
                  Date = yearquarter(Date)) |> 
  select(-Month)

Realkreditudlån_2013_2023 <- read_excel("Data/Realkreditudlån 2013 - 2023 Monthly.xlsx", 
                                        skip = 2)
  
Realkreditudlån_2 = Realkreditudlån_2013_2023 |> 
  select(-...1, -...2, -...3, -...4, -...5, -...6) |> 
  rename("Month" = '...7',
         'Udlån' = 'I alt') |> 
  filter(Udlån != '..') |> 
  mutate(Date = seq(as.Date("2013-10-01"), as.Date("2022-12-01"), by = "1 month"),
         Date = yearquarter(Date),
         Udlån = as.double(Udlån)) |> 
  select(-Month)

Realkreditudlån_2022_2024 <- read_excel("Data/Realkreditudlån 2022 - 2024 Monthly.xlsx", 
                                                skip = 2)

Realkreditudlån_3 = Realkreditudlån_2022_2024 |> 
  select(-...1, -...2, -...3, -...4, -...5, -...6) |> 
  rename("Month" = '...7',
         'Udlån' = `Ejerbolig og fritidshuse`) |> 
  filter(Udlån != '..') |> 
  mutate(Date = seq(as.Date("2022-12-01"), as.Date("2024-02-01"), by = "1 month"),
         Date = yearquarter(Date)) |> 
  select(-Month)
  
Realkreditudlån = Realkreditudlån_1 |> 
  bind_rows(Realkreditudlån_2) |> 
  bind_rows(Realkreditudlån_3) |> 
  distinct(Date) |> 
  as_tsibble(index = "Date")
  
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

left_join(Area, by = c("Kommune_2" = "Kommune", "Year"))
  
Korrespondancetabel_Kommuner <- read_excel("Data/Korrespondancetabel Kommuner.xlsx")

Korrespondancetabel_Kommuner

Permits_Region_1993_2006 <- read_excel("Data/Permits Region 1993-2006.xlsx", skip = 2)

Permits_Region_1993_2006 = Permits_Region_1993_2006 |> 
  select(-...1,-...2,-...3) |> 
  rename(Kommune = ...4) |> 
  pivot_longer(cols = "1981":"2006",
               names_to = "Year",
               values_to = "Permits") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "AMT_KOM_TXT")) |> 
  filter(!is.na(NUTS_KODE)) |> 
  select(Year, Permits, NUTS_KODE) |> 
  mutate(Year = as.integer(Year)) |> 
  group_by(Year, NUTS_KODE) |> 
  mutate(Permits = sum(Permits)) |> 
  distinct(Year, NUTS_KODE, .keep_all = TRUE) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")

Permits_Region_2006_2024 <- read_excel("Data/Permits Region 2006-2024.xlsx", 
                                       skip = 2) 

Permits_Region_2006_2024 = Permits_Region_2006_2024 |> 
  select(-...1,-...2,-...3) |> 
  rename(Kommune = ...4) |> 
  pivot_longer(cols = "2006Q1":"2023Q4",
               names_to = "Year",
               values_to = "Permits") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Lyngby-Taarbæk", "Lyngby-Tårbæk", Kommune)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "NUTS_TXT")) |> 
  select(Year, Permits, NUTS_KODE) |> 
  mutate(Year = as.factor(substr(Year, 1,4)),
         Permits = Permits) |> 
  group_by(Year, NUTS_KODE) |> 
  summarise(Permits = sum(Permits, na.rm = TRUE)) |> 
  mutate(Year = as.character(Year),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year") |> 
  filter(Year != "2006")

Permits = Permits_Region_1993_2006 |> 
  bind_rows(Permits_Region_2006_2024)

Immigration_Region_1980_2005 <- read_excel("Data/Immigration Region 1980 - 2005.xlsx", 
                                           skip = 2)

Immigration_Region_1980_2005 = Immigration_Region_1980_2005 |> 
  rename(Kommune = ...1) |> 
  pivot_longer(cols = "1980":"2005",
               names_to = "Year",
               values_to = "Immigration") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Lyngby-Taarbæk", "Lyngby-Tårbæk", Kommune)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "NUTS_TXT")) |> 
  select(Year, Immigration, NUTS_KODE) |> 
  group_by(Year, NUTS_KODE) |> 
  summarise(Immigration = sum(Immigration, na.rm = TRUE)) |> 
  mutate(Year = as.character(Year),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")

Immigration_Region_2006 <- read_excel("Data/Immigration Region 2006.xlsx", 
                                      skip = 2)

Immigration_Region_2006 = Immigration_Region_2006 |> 
  select(-...1) |> 
  rename(Kommune = ...2,
         Immigration = "2006") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Lyngby-Taarbæk", "Lyngby-Tårbæk", Kommune),
         Year = as.integer(2006)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "NUTS_TXT")) |> 
  select(Year, Immigration, NUTS_KODE) |> 
  distinct(Year, NUTS_KODE, .keep_all = TRUE) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")

Immigration_Region_2007_2023 <- read_excel("Data/Immigration Region 2007 - 2023.xlsx", 
                                           skip = 2)

Immigration_Region_2007_2023 = Immigration_Region_2007_2023 |> 
  rename(Kommune = ...1) |> 
  pivot_longer(cols = "2007":"2023",
               names_to = "Year",
               values_to = "Immigration") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Lyngby-Taarbæk", "Lyngby-Tårbæk", Kommune),
         Year = as.integer(Year)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "NUTS_TXT")) |> 
  select(Year, Immigration, NUTS_KODE) |> 
  distinct(Year, NUTS_KODE, .keep_all = TRUE) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")

Immigration = Immigration_Region_1980_2005 |> 
  bind_rows(Immigration_Region_2006) |> 
  bind_rows(Immigration_Region_2007_2023)

Houses_for_sale_Region_2004_2020 <- read_excel("Data/Houses for sale Region 2004-2020.xlsx", 
                                               skip = 2)
Houses_for_sale_Region_2004_2020 = Houses_for_sale_Region_2004_2020 |> 
  select(-...1) |> 
  rename(Kommune = ...2) |> 
  pivot_longer(cols = "2004M01":"2020M12",
               names_to = "Month",
               values_to = "Houses_for_sale") |> 
  group_by(Kommune) |> 
  mutate(Date = seq(as.Date("2004-01-01"), as.Date("2020-12-01"), by = "1 month"),
                Date = yearquarter(Date)) |> 
  ungroup() |> 
  select(Kommune, Houses_for_sale, Date) |> 
  group_by(Kommune, Date) |> 
  mutate(Houses_for_sale = sum(Houses_for_sale)) |> 
  distinct(Kommune, Date, .keep_all = TRUE) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Date")

Houses_for_sale_Region_2021_2024 <- read_excel("Data/Houses for sale Region 2021-2024.xlsx", 
                                               skip = 2)

Houses_for_sale_Region_2021_2024 = Houses_for_sale_Region_2021_2024 |> 
  select(-...1) |> 
  rename(Kommune = ...2) |> 
  pivot_longer(cols = "2021M01":"2024M02",
               names_to = "Month",
               values_to = "Houses_for_sale") |> 
  group_by(Kommune) |> 
  mutate(Date = seq(as.Date("2021-01-01"), as.Date("2024-02-01"), by = "1 month"),
         Date = yearquarter(Date)) |> 
  ungroup() |> 
  select(Kommune, Houses_for_sale, Date) |> 
  group_by(Kommune, Date) |> 
  mutate(Houses_for_sale = sum(Houses_for_sale)) |> 
  distinct(Kommune, Date, .keep_all = TRUE) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Date")

Houses_for_sale = Houses_for_sale_Region_2004_2020 |> 
  bind_rows(Houses_for_sale_Region_2021_2024)

Construction_Cost_Index_1986_2015 <- read_excel("Data/Construction Cost Index 1986 - 2015.xlsx", 
                                                skip = 2) 
  
Construction_Cost_Index_1986_2015 = Construction_Cost_Index_1986_2015 |> 
  select(-...1,-...2,-...3) |> 
  pivot_longer(cols = "1986Q4":"2015Q4",
               names_to = "Date",
               values_to = "CCI") |> 
  mutate(CCI = CCI * (100 / 135.75),
         Date = yearquarter(Date)) |> 
  slice(1:65) |> 
  as_tsibble(index = "Date")

CCI <- read_excel("Data/Construction cost index National Quarterly.xlsx", 
                  skip = 2) |> 
  select(-...1, -...2, -...3, -...4) |> 
  filter(`2003Q1` != is.na(`2003Q1`)) |> 
  pivot_longer(cols = "2003Q1":"2023Q4",
               names_to = "Date",
               values_to = "CCI") |> 
  mutate(Date = yearquarter(Date)) |>
  as_tsibble(index = "Date")
  
Construction_Cost_Index = Construction_Cost_Index_1986_2015 |> 
  bind_rows(CCI) |> 
  mutate(diff_CCI = CCI - lag(CCI))

### Simple XGBoost model

data_train = Ejendomspriser |> 
  filter(Date < yearquarter(	
    "2010 Q4"))

data_test = data_train = Ejendomspriser |> 
  filter(Date > yearquarter(	
    "2010 Q4"))

library(dynamac) # for dynamic linear models (ARDL)
library(forecast) # for forecasting
library(dplyr) # for data manipulation

# Assuming 'data' is your data.frame with all municipalities' time series data
# 'Kommune' is the column indicating the municipality
# The dependent variable and explanatory variables are also columns in this data.frame
dependent_var <- "Growth_rate"
explanatory_vars <- c("diff_Rate", "diff_log_Disp_Income")
kommune_var <- "Kommune"

# Unique list of municipalities
municipalities <- unique(Ejendomspriser$Kommune)

# Placeholder for all combined forecasts across municipalities
all_municipalities_combined_forecasts <- list()
municipality_data <- list()
i = 1

# Loop through each municipality

for (municipality in municipalities) {
  # Filter data for the current municipality and store it in the list
  municipality_data[[i]] <- Ejendomspriser |> filter(Kommune == municipality)
  
  # Placeholder for combined forecasts for the current municipality
  combined_forecasts <- list()
  
  # Expanding window parameters
  start_window <- 20
  end_window <- nrow(municipality_data[[i]])
  forecast_horizon <- 1
  
  for (time in seq(start_window, end_window, by = 1)) {
    window_data <- municipality_data[[i]][1:time, ] # Expanding window data for the municipality
    
    date <- municipality_data[[i]][1:time, ] |> select(Date) |> tail(1) + 1
    
    # Store individual forecasts from each model for this window
    window_forecasts <- numeric(length(explanatory_vars))
    j <- 1
    
    for (var in explanatory_vars) {
      
      # Define the ARDL model formula
      model_formula <- reformulate(c(paste0((var))), response = dependent_var)
      
      lags_list <- setNames(list(1, 1), c("Growth_rate", var))
      
      # Run ARDL model for the municipality
      model <- dynardl(model_formula, data = window_data, 
                       lags = lags_list,
                       ec = TRUE, simulate = FALSE)
      
      # Forecasting
      forecast_result <- model$model$coefficients[[1]] + 
        model$model$coefficients[[2]] * tail(model$model$model$l.1.Growth_rate, 1)[1] +
        model$model$coefficients[[3]] * tail(model$model$model[,3],1)[1]
      
      # Storing the forecast from the current explanatory variable
      window_forecasts[j] <- forecast_result
      
      j <- j + 1
      
    }
    
    # Combining the forecasts (e.g., taking the mean of individual forecasts)
    combined_forecasts[[time]] <- data.frame(
      Date = date,
      CombinedForecast = mean(window_forecasts)
    )
  }
  
  # Store combined forecasts for the current municipality
  all_municipalities_combined_forecasts[[municipality]] <- combined_forecasts
  
  # Increment the counter
  i <- i + 1
}

# 'all_municipalities_combined_forecasts' now contains the combined forecasts for each municipality

for (var in explanatory_vars) {
  model_formula <- reformulate(c(paste0((var))), response = dependent_var)
  print(model_formula)
  model <- dynardl(formula = model_formula, data = window_data)
  print(model$model)
}


Construction_Cost_Index_1986_2015 <- read_excel("Data/Construction Cost Index 1986 - 2015.xlsx", 
                                                skip = 2) 

Construction_Cost_Index_1986_2015 = Construction_Cost_Index_1986_2015 |> 
  select(-...1,-...2,-...3) |> 
  pivot_longer(cols = "1986Q4":"2015Q4",
               names_to = "Date",
               values_to = "CCI") |> 
  mutate(CCI = CCI * (100 / 135.75),
         Date = yearquarter(Date)) |> 
  dplyr::slice(1:65) |> 
  as_tsibble(index = "Date")


         
         
Interest_rates = read_excel("Data/Udlåns rente Monthly.xlsx", 
                                     skip = 2) |> 
           select(-...1,-...2) |> 
           rename(Rate = Månedsultimo) |>
           filter(Rate != "..") |> 
           mutate(Date = seq(as.Date("1992-4-01"), as.Date("2024-02-01"), by = "1 month"),
                  Date = yearquarter(Date)) |>
           mutate(Rate = as.double(Rate)) |> 
           filter(Rate != is.na(Rate)) |> 
           group_by(Date) |> 
           summarise(Rate = mean(Rate)) |> 
           mutate(diff_Rate = Rate - lag(Rate)) |> 
           as_tsibble(index = "Date") |> 
           left_join(CPI, by = "Date") |> 
           filter(Index != is.na(Index)) |> 
           mutate(Real_Rate = (Rate / Index))




Gross_Lending <- read_excel("Data/Mortgage Banks Gross Lending - Quarterly National.xlsx", 
                            skip = 2) |> 
  select(-...1,-...2) |> 
  rename(Date = ...3,
         Gross_Lending = "All loan types") |> 
  mutate(Date = yearquarter(Date),
         diff_Gross_Lending = Gross_Lending - lag(Gross_Lending)) |> 
  as_tsibble(index = "Date") |> 
  left_join(CPI, by = "Date") |>
  filter(Index != is.na(Index)) |>
  mutate(Real_Gross_Lending = (Gross_Lending / Index)) |>
  select(-Year, -Index)

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


Ejendomspriser |> 
  as.tibble() |> 
  group_by(Kommune) |> 
  select(-Kommune_2, -Kommune_ID, -Kom.nr.x, -Vacant_Houses, -Index, -House_Price_CPI, -Disp_Income_CPI)


ADF$type1 |> 
  as.tibble() |> 
  rbind(ADF$type2 |> 
          as.tibble()) |> 
  rbind(ADF$type3 |> 
          as.tibble()) |> 
  mutate(test = 1)

bind_rows(
  ADF$type1 %>% as_tibble() %>% mutate(test = 1),
  ADF$type2 %>% as_tibble() %>% mutate(test = 2),
  ADF$type3 %>% as_tibble() %>% mutate(test = 3)) |> 
  group_by(test) |> 
  gt()




W = 30
i = 0
j = 
  E = Ejendomspriser |> 
  as.tibble() |> 
  group_by(Date) |> 
  summarise(Growth_rate = mean(Growth_rate)) |> 
  filter(Growth_rate != is.na(Growth_rate)) |> 
  as_tsibble(index = Date) |> 
  nrow()

Ejendomspriser_tbl <- Ejendomspriser %>%
  as_tibble() %>%
  group_by(Date) %>%
  summarise(Growth_rate = mean(Growth_rate, na.rm = TRUE)) %>%
  filter(!is.na(Growth_rate)) %>%
  as_tsibble(index = Date)

fc <- vector("list", length = E-W)

for(i in 2:(E-W)) {
  fc[[i]] <- Ejendomspriser_tbl %>%
    dplyr::slice(1:(W+i-1)) %>%
    model(ARMA_400 = ARIMA(Growth_rate ~ pdq(4,0,0)),
          ARMA_100 = ARIMA(Growth_rate ~ pdq(1,0,0))) %>%
    fabletools::forecast(h = 1) %>%
    as_tibble()
}

fc_combined <- bind_rows(fc)

fc_combined |> 
  dplyr::bind_cols(Ejendomspriser |> 
                     as.tibble() |> 
                     group_by(Date) |> 
                     summarise(Growth_rate = mean(Growth_rate)) |> 
                     filter(Growth_rate != is.na(Growth_rate)) |> 
                     as_tsibble(index = Date) |> 
                     dplyr::slice((W+1):E) |> 
                     select(Growth_rate)) |> 
  rename(Truth = Growth_rate...5,
         Forecast = .mean) |> 
  select(.model, Forecast, Truth) |> 
  yardstick::rmse(Truth, Forecast)


#| label: Best model based on RMSE for model specifications

kommune_var <- "Kommune"
municipalities <- unique(Ejendomspriser$Kommune)

# Placeholder for all combined forecasts across municipalities
prediction_full <- tibble()

# Using foreach to loop through municipalities in parallel
results <- foreach(municipality = municipalities, .combine = bind_rows, .packages = c("dplyr", "fable", "fabletools", "tsibble")) %dopar% {
  municipality_data <- Ejendomspriser |> filter(Kommune == municipality)
  
  prediction <- tibble()
  
  # Expanding window parameters
  start_window <- 20
  end_window <- nrow(municipality_data)
  forecast_horizon <- 1
  
  for (time in seq(start_window, end_window, by = 1)) {
    window_data <- municipality_data[1:time, ]
    date <- window_data |> select(Date) |> tail(1) |> mutate(Date = Date + 1)
    
    forecasts <- window_data |>
      model(
        ARIMA_001 = ARIMA(Growth_rate ~ pdq(0,0,1) + PDQ(0,0,0)),
        ARIMA_100 = ARIMA(Growth_rate ~ pdq(1,0,0) + PDQ(0,0,0)),
        ARIMA_001_mean = ARIMA(Growth_rate ~ 0 + pdq(0,0,1) + PDQ(0,0,0))
      ) |> 
      forecast(h = forecast_horizon)
    
    prediction <- bind_rows(prediction, forecasts)
  }
  
  prediction
}

setwd("C:/Users/asger/OneDrive - Aarhus universitet/Economic Forecasting/House-Prices")

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
  mutate(Growth_rate = log_House_Price_CPI - lag(log_House_Price_CPI)) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Date")

read_excel("Data/Employmet Rate National Quarterly.xls", 
                             skip = 10) |> 
  rename(Date = observation_date,
         Employment_Rate = LREM64TTDKQ156S) |> 
  mutate(Date = yearquarter(Date),
         Date = year(Date)) |> 
  group_by(Date) |> 
  summarise(Employment_Rate = mean(Employment_Rate)) |> 
  as_tsibble(index = Date)

Houses_for_sale_Region_2004_2020 <- read_excel("Data/Houses for sale Region 2004-2020.xlsx", 
                                               skip = 2)

Houses_for_sale_Region_2004_2020 = Houses_for_sale_Region_2004_2020 |> 
  select(-...1) |> 
  rename(Kommune = ...2) |> 
  pivot_longer(cols = "2004M01":"2020M12",
               names_to = "Month",
               values_to = "Houses_for_sale") |> 
  group_by(Kommune) |> 
  mutate(Date = seq(as.Date("2004-01-01"), as.Date("2020-12-01"), by = "1 month"),
         Date = yearquarter(Date)) |> 
  ungroup() |> 
  select(Kommune, Houses_for_sale, Date) |> 
  group_by(Kommune, Date) |> 
  mutate(Houses_for_sale = sum(Houses_for_sale)) |> 
  distinct(Kommune, Date, .keep_all = TRUE) |> 
  ungroup() |> 
  as_tsibble(key = "Kommune", index = "Date")

Interest_rates = read_excel("Data/Udlåns rente Monthly.xlsx", 
                            skip = 2) |> 
  select(-...1,-...2) |>
  rename(Rate = Månedsultimo) |>
  filter(Rate != "..") |>
  mutate(Date = seq(as.Date("1992-4-01"), as.Date("2024-02-01"), by = "1 month"),
         Date = yearquarter(Date)) |>
  mutate(Rate = as.double(Rate)) |>
  filter(Rate != is.na(Rate)) |>
  group_by(Date) |>
  summarise(Rate = mean(Rate)) |>
  mutate(diff_Rate = Rate - lag(Rate)) |>
  as_tsibble(index = "Date") |>
  left_join(CPI, by = "Date") |>
  filter(Index != is.na(Index)) |>
  mutate(Real_Rate = (Rate / Index)) |>
  select(-Year, -Index)

Ejendomspriser |> 
  left_join(Key_for_Regions, by = "Kommune") |> 
  mutate(Kom.nr = case_when(Kommune == "Århus" ~ 751,
                            Kommune != "Århus" ~ Kom.nr)) |> 
  left_join(Disposable_Income, by = c("Kommune" = "Kommune", "Date")) |> 
  left_join(Consumer_Confidence, by = "Date") |> 
  left_join(Construction_Cost_Index, by = "Date") |> 
  left_join(Employmet_Rate, by = "Date") |> 
  left_join(GDP, by = "Date") |>
  left_join(Gross_Lending, by = "Date") |>
  left_join(Grundskyld, by = c("Kom.nr", "Date")) |> 
  left_join(Household, by = c("Kommune", "Date")) |> 
  left_join(Houses_sold, by = c("Kommune", "Date")) |> 
  left_join(Immigration, by = c("Date", "Kom.nr" = "NUTS_KODE")) |>
  left_join(Population, by = c("Kom.nr", "Date")) |> 
  left_join(Price_to_Income_Ratio, by = "Date") |> 
  left_join(Price_to_Rent_ratio, by = "Date") |> 
  left_join(Rent_Prices, by = "Date") |> 
  left_join(Anmeldte_voldsforbrydelser, by = c("Kom.nr", "Date")) |> 
  left_join(Anmeldte_indbrud, by = c("Kom.nr", "Date")) |> 
  left_join(Andel_Ejerboliger, by = c("Kom.nr", "Date")) |> 
  left_join(Density, by = c("Kom.nr", "Date")) |> 
  left_join(Udskrivningsprocent, by = c("Kom.nr", "Date")) |> 
  left_join(Udgift_til_dagtilbud, by = c("Kom.nr", "Date")) |> 
  left_join(Socioøkonomiske_indeks, by = c("Kom.nr", "Date")) |> 
  left_join(Interest_rates, by = "Date") |> 
  left_join(Permits, by = c("Date", "Kom.nr" = "NUTS_KODE"))

Korrespondancetabel_Kommuner <- read_excel("Data/Korrespondancetabel Kommuner.xlsx")

Korrespondancetabel_Kommuner

explanatory_vars <- c("Growth_rate", "Disp_Income", "Consumer_Confidence", "CCI", "Unemployment_rate",
                      "GDP", "Gross_Lending", "Grundskyld", "Households", "Houses_sold", "Immigration",
                      "Population", "PTIR", "PTRR", "Rent_Price", "Anmeldte_voldsforbrydelser", "Anmeldte_indbrud",
                      "Density", "Udskrivningsprocent", "Udgift_til_dagtilbud", "Socioøkonomiske_indeks",
                      "Interest_Rate", "Permits")

Ejendomspriser |> 
  mutate(across(all_of(explanatory_vars), ~c(NA, diff(.x)), .names = "diff_{.col}")) |> 
  View()




Houses_sold_1992_2002 <- read_excel("Data/Ejendomssalg 1992-2002.xlsx", 
                                    skip = 2) |> 
  rename(Kommune_OLD = ...4) |> 
  select(-...1,-...2,-...3) |> 
  pivot_longer(cols = "1992K1":"2002K4",
               names_to = "Date",
               values_to = "Houses_sold") |> 
  mutate(Date = yearquarter(Date),
         Date = year(Date)) |> 
  group_by(Kommune_OLD, Date) |> 
  summarise(Houses_sold = sum(Houses_sold)) |> 
  ungroup() |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune_OLD" = "AMT_KOM_TXT"), relationship = "many-to-many") |> 
  filter(NUTS_KODE != is.na(NUTS_KODE)) |>
  rename(Kommune = NUTS_TXT) |> 
  select(Date, Houses_sold, Kommune) |> 
  group_by(Date, Kommune) |> 
  summarise(Houses_sold = sum(Houses_sold)) |> 
  mutate(Kommune = if_else(Kommune == "Lyngby-Tårbæk", "Lyngby-Taarbæk", Kommune),
         Kommune = if_else(Kommune == "Aarhus", "Århus", Kommune),
         Kommune = if_else(Kommune == "Copenhagen", "København", Kommune)) |> 
  as_tsibble(key = "Kommune", index = "Date")

Houses_sold_2003 <- data.frame(
  Date = rep(2003, n_distinct(Houses_sold_1992_2002$Kommune)),
  Houses_sold = NA,
  Kommune = unique(Houses_sold_1992_2002$Kommune))|>
  as_tsibble(key = "Kommune", index = "Date")

Houses_sold_1992_2003 <- Houses_sold_1992_2002 |> 
  bind_rows(Houses_sold_2003) |>
  as.tibble() |> 
  group_by(Kommune) |> 
  mutate(Houses_sold = round(if_else(is.na(Houses_sold), mean(Houses_sold, na.rm = TRUE), Houses_sold),2)) |> 
  as_tsibble(key = "Kommune", index = "Date")
  
Population_raw |> 
  filter(Kom.nr != is.na(Kom.nr)) |> 
  select(-...1) |> 
  pivot_longer("1993":"2024",
               names_to = "Date",
               values_to = "Population") |> 
  left_join(Korrespondancetabel_Kommuner |> 
              select(NUTS_KODE, NUTS_TXT) |> 
              unique(), by = c("Kom.nr" = "NUTS_KODE")) |> 
  select(-Kom.nr) |> 
  rename(Kommune = NUTS_TXT) |> 
  mutate(Date = as.integer(Date)) |> 
  as_tsibble(key = "Kommune", index = "Date") |> View()


Grundskyld <- read_excel("Data/Grundskylds_Promille Regional Yearly.xlsx") |> 
  filter(Kom.nr != is.na(Kom.nr)) |> 
  select(-...1, -'2024') |> 
  mutate(across(all_of(column_names), as.double)) |> 
  pivot_longer(cols = "1993":"2023",
               names_to = "Date",
               values_to = "Grundskyld") |> 
  left_join(Korrespondancetabel_Kommuner |> 
              select(NUTS_KODE, NUTS_TXT) |> 
              unique(), by = c("Kom.nr" = "NUTS_KODE")) |> 
  select(-Kom.nr) |> 
  rename(Kommune = NUTS_TXT) |> 
  mutate(Date = as.integer(Date)) |> 
  as_tsibble(key = "Kommune", index = "Date")


Korrespondancetabel_Kommuner <- read_excel("Data/Korrespondancetabel Kommuner.xlsx")

Korrespondancetabel_Kommuner <- Korrespondancetabel_Kommuner |> 
  mutate(NUTS_TXT = if_else(NUTS_TXT == "Lyngby-Tårbæk", "Lyngby-Taarbæk", NUTS_TXT))

Permits_Region_1993_2006 <- read_excel("Data/Permits Region 1993-2006.xlsx", skip = 2)

Permits_Region_1993_2006 = Permits_Region_1993_2006 |> 
  select(-...1,-...2,-...3) |> 
  rename(Kommune = ...4) |> 
  pivot_longer(cols = "1981":"2006",
               names_to = "Year",
               values_to = "Permits") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "AMT_KOM_TXT")) |> 
  filter(!is.na(NUTS_KODE)) |> 
  select(Year, Permits, NUTS_KODE) |> 
  mutate(Year = as.integer(Year)) |> 
  group_by(Year, NUTS_KODE) |> 
  mutate(Permits = sum(Permits)) |> 
  distinct(Year, NUTS_KODE, .keep_all = TRUE) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")


Ejendomspriser |> 
  left_join(Korrespondancetabel_Kommuner |> 
              select(NUTS_KODE, NUTS_TXT) |> 
              rename(Kommune = NUTS_TXT,
                     Kom.nr = NUTS_KODE) |> 
              unique(), by = "Kommune") |> 
  left_join(Disposable_Income, by = c("Kommune" = "Kommune", "Date")) |> 
  left_join(Consumer_Confidence, by = "Date") |> 
  left_join(Construction_Cost_Index, by = "Date") |> 
  left_join(Employmet_Rate, by = "Date") |> 
  left_join(GDP, by = "Date") |>
  left_join(Gross_Lending, by = "Date") |>
  left_join(Grundskyld, by = c("Kommune", "Date")) |> 
  left_join(Household, by = c("Kommune", "Date")) |> 
  left_join(Houses_sold, by = c("Kommune", "Date")) |> 
  left_join(Immigration, by = c("Date", "Kom.nr" = "NUTS_KODE")) |>
  left_join(Population, by = c("Kommune", "Date")) |> 
  left_join(Price_to_Income_Ratio, by = "Date") |> 
  left_join(Price_to_Rent_ratio, by = "Date") |> 
  left_join(Rent_Prices, by = "Date") |> 
  left_join(Interest_rates, by = "Date") |> 
  left_join(Permits, by = c("Date", "Kom.nr" = "NUTS_KODE")) |> 
  ungroup()

Anmeldte_voldsforbrydelser <- Anmeldte_voldsforbrydelser_raw |> 
  mutate(across(matches('^(199[3-9]|20[0-1][0-9]|202[0-3])$'), as.double))|> 
  select(-...1, -"2024", -"2023") |> 
  pivot_longer(, cols = "1993":"2022",
               names_to = "Date",
               values_to = "Anmeldte_voldsforbrydelser") |> 
  filter(!is.na(Kom.nr)) |>
  mutate(Kom.nr = as.double(Kom.nr),
         Date = as.integer(Date),
         Kommune = if_else(Kommune == "Lyngby-Tårbæk", "Lyngby-Taarbæk", Kommune)) |> 
  group_by(Kom.nr, Date) |> 
  mutate(Anmeldte_voldsforbrydelser = if_else(is.na(Anmeldte_voldsforbrydelser),
                                              mean(Anmeldte_voldsforbrydelser), Anmeldte_voldsforbrydelser)) |> 
  select(-Kommune) |> 
  as_tsibble(key = "Kom.nr", index = "Date")

Immigration_Region_1980_2005 <- read_excel("Data/Immigration Region 1980 - 2005.xlsx", 
                                           skip = 2)

Immigration_Region_1980_2005 = Immigration_Region_1980_2005 |> 
  rename(Kommune = ...1) |> 
  pivot_longer(cols = "1980":"2005",
               names_to = "Year",
               values_to = "Immigration") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Lyngby-Tårbæk", "Lyngby-Tårbæk", Kommune)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "AMT_KOM_TXT")) |> 
  select(Year, Immigration, NUTS_KODE) |> 
  group_by(Year, NUTS_KODE) |> 
  summarise(Immigration = sum(Immigration, na.rm = TRUE)) |> 
  mutate(Year = as.character(Year),
         Year = as.integer(Year)) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year") |> 
  filter(NUTS_KODE != is.na(NUTS_KODE))

Immigration_Region_2006 <- read_excel("Data/Immigration Region 2006.xlsx", 
                                      skip = 2)

Immigration_Region_2006 = Immigration_Region_2006 |> 
  select(-...1) |> 
  rename(Kommune = ...2,
         Immigration = "2006") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Aarhus", "Århus", Kommune),
         Year = as.integer(2006)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "NUTS_TXT")) |> 
  select(Year, Immigration, NUTS_KODE) |> 
  distinct(Year, NUTS_KODE, .keep_all = TRUE) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")

Immigration_Region_2007_2023 <- read_excel("Data/Immigration Region 2007 - 2023.xlsx", 
                                           skip = 2)

Immigration_Region_2007_2023 = Immigration_Region_2007_2023 |> 
  rename(Kommune = ...1) |> 
  pivot_longer(cols = "2007":"2023",
               names_to = "Year",
               values_to = "Immigration") |> 
  mutate(Kommune = if_else(Kommune == "Copenhagen", "København", Kommune),
         Kommune = if_else(Kommune == "Aarhus", "Århus", Kommune),
         Year = as.integer(Year)) |> 
  left_join(Korrespondancetabel_Kommuner, by = c("Kommune" = "NUTS_TXT")) |> 
  select(Year, Immigration, NUTS_KODE) |> 
  distinct(Year, NUTS_KODE, .keep_all = TRUE) |> 
  as_tsibble(key = "NUTS_KODE", index = "Year")

Immigration = Immigration_Region_1980_2005 |> 
  bind_rows(Immigration_Region_2006) |> 
  bind_rows(Immigration_Region_2007_2023) |> 
  rename(Date = Year)

explanatory_vars <- c("Disp_Income", "Consumer_Confidence", "CCI", "Unemployment_rate",
                      "GDP", "Gross_Lending", "Grundskyld", "Households", "Houses_sold", "Immigration",
                      "Population", "PTIR", "PTRR", "Rent_Price", "Anmeldte_voldsforbrydelser", "Anmeldte_indbrud",
                      "Density", "Udskrivningsprocent", "Udgift_til_dagtilbud", "Socioøkonomiske_indeks",
                      "Interest_Rate", "Permits")
dependent_var <- "Growth_rate"
diff_dependent_var <- "diff_Growth_rate"



add_dynamic_lags <- function(data, var_lag_list) {
  for (var_lag in var_lag_list) {
    
    var_name <- var_lag
    mod <- model_spec |> 
      filter(Variable == var_name) |> 
      select('model$best_order') |> as.data.frame() 
    
    lags <- mod[[1]]
    
    for (lag in 1:lags) {
      lagged_col_name <- paste0("L(", var_name,", ", lag, ")")
      data[[lagged_col_name]] <- dplyr::lag(data[[var_name]], lag)
    }
    
  }
  
  return(data)
  
}


j <- 1

ARDL_results <- foreach (municipality = municipalities[1], .packages = c("dplyr", "ARDL", "tidyverse", "tibble", "stats")) %dopar% {
  # Filter data for the current municipality and store it in the list
  municipality_data[[i]] <- Ejendomspriser |> filter(Kommune == municipality)
  
  # Placeholder for combined forecasts for the current municipality
  combined_forecasts <- list()
  
  # Expanding window parameters
  start_window <- 20
  end_window <- nrow(municipality_data[[i]])
  forecast_horizon <- 1
  Dato <- list()
  j <- 1
  forecasted_value <- list()
  
  for (time in seq(start_window, end_window, by = 1)) {
    
    window_data <- municipality_data[[i]][1:(time), ] # Expanding window data for the municipality
    
    date <- municipality_data[[i]][1:time, ] |> dplyr::select(Date) |> tail(1) + 1
    
    # Store individual forecasts from each model for this window
    window_forecasts <- numeric(length(explanatory_vars))
    forecast <- list()
    l <- 1
    
    for (var in explanatory_vars) {
      
      # Define the ARDL model formula
      Growth_rate_diff <- Growth_rate_adf |> filter(municipality_name == municipality)
      var_diff <- test |> filter(municipality_name == municipality & variable_name == var)
      
      model_formula_0_0 <- reformulate(c(paste0((var))), response = dependent_var)
      model_formula_1_0 <- reformulate(c(paste0((var))), response = diff_dependent_var)
      model_formula_0_1 <- reformulate(c(paste0("diff_", (var))), response = dependent_var)
      model_formula_1_1 <- reformulate(c(paste0("diff_", (var))), response = diff_dependent_var)
      
      if(Growth_rate_diff$p_value == TRUE) {
        
        if(var_diff$p_value == TRUE) {
          
          model <- auto_ardl(model_formula_0_0, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        } else {
          
          model <- auto_ardl(model_formula_0_1, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        }
        
      } else {
        
        if(var_diff$p_value == TRUE) {
          
          model <- auto_ardl(model_formula_1_0, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        } else {
          
          model <- auto_ardl(model_formula_1_1, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        }
        
        
      }
      
      forecast_data <- municipality_data[[i]][1:(time), ]
      model_spec <- rownames_to_column(as.data.frame(model$best_order), var = "Variable")
      var_lag_list <- list(model_spec$Variable[1], model_spec$Variable[2])
      data <- add_dynamic_lags(forecast_data, var_lag_list)
      lm_model <- to_lm(model$best_model)
      forecast[l] <- predict(lm_model, data) |> tail(1)
      
      l <- l + 1
      
    }
    
    forecasted_value[j] <- mean(unlist(forecast))
    Dato[j] <- date
    
    j <- j + 1
    
  }

  forecasted_values <- as_tibble(cbind(unlist(Dato), unlist(forecasted_value)))
  table <- cbind(municipality, forecasted_values)
    
} 

system.time({
  foreach (municipality = municipalities[1:2], .packages = c("dplyr", "ARDL", "tidyverse", "tibble", "stats")) %dopar% {
    # Filter data for the current municipality and store it in the list
    municipality_data[[i]] <- Ejendomspriser |> filter(Kommune == municipality)
    
    # Placeholder for combined forecasts for the current municipality
    combined_forecasts <- list()
    
    # Expanding window parameters
    start_window <- 20
    end_window <- nrow(municipality_data[[i]])
    forecast_horizon <- 1
    Dato <- list()
    j <- 1
    forecasted_value <- list()
    
    for (time in seq(start_window, end_window, by = 1)) {
      
      window_data <- municipality_data[[i]][1:(time), ] # Expanding window data for the municipality
      
      date <- municipality_data[[i]][1:time, ] |> dplyr::select(Date) |> tail(1) + 1
      
      # Store individual forecasts from each model for this window
      window_forecasts <- numeric(length(explanatory_vars))
      forecast <- list()
      l <- 1
      
      for (var in explanatory_vars) {
        
        # Define the ARDL model formula
        Growth_rate_diff <- Growth_rate_adf |> filter(municipality_name == municipality)
        var_diff <- test |> filter(municipality_name == municipality & variable_name == var)
        
        model_formula_0_0 <- reformulate(c(paste0((var))), response = dependent_var)
        model_formula_1_0 <- reformulate(c(paste0((var))), response = diff_dependent_var)
        model_formula_0_1 <- reformulate(c(paste0("diff_", (var))), response = dependent_var)
        model_formula_1_1 <- reformulate(c(paste0("diff_", (var))), response = diff_dependent_var)
        
        if(Growth_rate_diff$p_value == TRUE) {
          
          if(var_diff$p_value == TRUE) {
            
            model <- auto_ardl(model_formula_0_0, data = window_data, 
                               max_order = c(4,4),
                               starting_order = c(1,1), selection = "BIC")
            
          } else {
            
            model <- auto_ardl(model_formula_0_1, data = window_data, 
                               max_order = c(4,4),
                               starting_order = c(1,1), selection = "BIC")
            
          }
          
        } else {
          
          if(var_diff$p_value == TRUE) {
            
            model <- auto_ardl(model_formula_1_0, data = window_data, 
                               max_order = c(4,4),
                               starting_order = c(1,1), selection = "BIC")
            
          } else {
            
            model <- auto_ardl(model_formula_1_1, data = window_data, 
                               max_order = c(4,4),
                               starting_order = c(1,1), selection = "BIC")
            
          }
          
          
        }
        
        forecast_data <- municipality_data[[i]][1:(time), ]
        model_spec <- rownames_to_column(as.data.frame(model$best_order), var = "Variable")
        var_lag_list <- list(model_spec$Variable[1], model_spec$Variable[2])
        data <- add_dynamic_lags(forecast_data, var_lag_list)
        lm_model <- to_lm(model$best_model)
        forecast[l] <- predict(lm_model, data) |> tail(1)
        
        l <- l + 1
        
      }
      
      forecasted_value[j] <- mean(unlist(forecast))
      Dato[j] <- date
      
      j <- j + 1
      
    }
    
    forecasted_values <- as_tibble(cbind(unlist(Dato), unlist(forecasted_value)))
    table <- cbind(municipality, forecasted_values)
    
  }
})


foreach(time = seq(start_window, end_window, by = 1),
        .packages = c("dplyr", "ARDL", "tidyverse", "tibble", "stats", "parallel")) %dopar% {
          
          window_data <- municipality_data[[i]][1:(time), ] # Expanding window data for the municipality
          
          date <- municipality_data[[i]][1:time, ] |> dplyr::select(Date) |> tail(1) + 1
          
          # Store individual forecasts from each model for this window
          window_forecasts <- numeric(length(explanatory_vars))
          forecast <- list()
          l <- 1
          
          foreach(var = explanatory_vars, .packages = c("dplyr", "ARDL", "tidyverse", "tibble", "stats")) %dopar% {
            
            # Define the ARDL model formula
            Growth_rate_diff <- Growth_rate_adf |> filter(municipality_name == municipality)
            var_diff <- test |> filter(municipality_name == municipality & variable_name == var)
            
            model_formula_0_0 <- reformulate(c(paste0((var))), response = dependent_var)
            model_formula_1_0 <- reformulate(c(paste0((var))), response = diff_dependent_var)
            model_formula_0_1 <- reformulate(c(paste0("diff_", (var))), response = dependent_var)
            model_formula_1_1 <- reformulate(c(paste0("diff_", (var))), response = diff_dependent_var)
            
            if(Growth_rate_diff$p_value == TRUE) {
              
              if(var_diff$p_value == TRUE) {
                
                model <- auto_ardl(model_formula_0_0, data = window_data, 
                                   max_order = c(4,4),
                                   starting_order = c(1,1), selection = "BIC")
                
              } else {
                
                model <- auto_ardl(model_formula_0_1, data = window_data, 
                                   max_order = c(4,4),
                                   starting_order = c(1,1), selection = "BIC")
                
              }
              
            } else {
              
              if(var_diff$p_value == TRUE) {
                
                model <- auto_ardl(model_formula_1_0, data = window_data, 
                                   max_order = c(4,4),
                                   starting_order = c(1,1), selection = "BIC")
                
              } else {
                
                model <- auto_ardl(model_formula_1_1, data = window_data, 
                                   max_order = c(4,4),
                                   starting_order = c(1,1), selection = "BIC")
                
              }
              
              
            }
            
            forecast_data <- municipality_data[[i]][1:(time), ]
            model_spec <- rownames_to_column(as.data.frame(model$best_order), var = "Variable")
            var_lag_list <- list(model_spec$Variable[1], model_spec$Variable[2])
            data <- add_dynamic_lags(forecast_data, var_lag_list)
            lm_model <- to_lm(model$best_model)
            forecast <- predict(lm_model, data) |> tail(1)
            
            l <- l + 1
            
          }
          
          forecasted_value[j] <- mean(unlist(forecast))
          Dato[j] <- date
          
          j <- j + 1
          
        }


parallel::detectCores()
n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)

kommune_var <- "Kommune"
dependent_var <- "Growth_rate"

municipalities <- unique(Ejendomspriser$Kommune)
p_value <- list()
municipality_name <- list()
variable_name <- list()
results <- list()
i = 1


ARDL_results <- foreach (municipality = municipalities, .packages = c("dplyr", "ARDL", "tidyverse", "tibble", "stats")) %dopar% {
  # Filter data for the current municipality and store it in the list
  municipality_data[[i]] <- Ejendomspriser |> filter(Kommune == municipality)
  
  # Placeholder for combined forecasts for the current municipality
  combined_forecasts <- list()
  
  # Expanding window parameters
  start_window <- 20
  end_window <- nrow(municipality_data[[i]])
  forecast_horizon <- 1
  Dato <- list()
  j <- 1
  forecasted_value <- list()
  
  for (time in seq(start_window, end_window, by = 1)) {
    
    window_data <- municipality_data[[i]][1:(time), ] # Expanding window data for the municipality
    
    date <- municipality_data[[i]][1:time, ] |> dplyr::select(Date) |> tail(1) + 1
    
    # Store individual forecasts from each model for this window
    window_forecasts <- numeric(length(explanatory_vars))
    forecast <- list()
    l <- 1
    
    for (var in explanatory_vars) {
      
      # Define the ARDL model formula
      Growth_rate_diff <- Growth_rate_adf |> filter(municipality_name == municipality)
      var_diff <- test |> filter(municipality_name == municipality & variable_name == var)
      
      model_formula_0_0 <- reformulate(c(paste0((var))), response = dependent_var)
      model_formula_1_0 <- reformulate(c(paste0((var))), response = diff_dependent_var)
      model_formula_0_1 <- reformulate(c(paste0("diff_", (var))), response = dependent_var)
      model_formula_1_1 <- reformulate(c(paste0("diff_", (var))), response = diff_dependent_var)
      
      if(Growth_rate_diff$p_value == TRUE) {
        
        if(var_diff$p_value == TRUE) {
          
          model <- auto_ardl(model_formula_0_0, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        } else {
          
          model <- auto_ardl(model_formula_0_1, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        }
        
      } else {
        
        if(var_diff$p_value == TRUE) {
          
          model <- auto_ardl(model_formula_1_0, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        } else {
          
          model <- auto_ardl(model_formula_1_1, data = window_data, 
                             max_order = c(4,4),
                             starting_order = c(1,1), selection = "BIC")
          
        }
        
        
      }
      
      forecast_data <- municipality_data[[i]][1:(time), ]
      model_spec <- rownames_to_column(as.data.frame(model$best_order), var = "Variable")
      var_lag_list <- list(model_spec$Variable[1], model_spec$Variable[2])
      data <- add_dynamic_lags(forecast_data, var_lag_list)
      lm_model <- to_lm(model$best_model)
      forecast[l] <- predict(lm_model, data) |> tail(1)
      
      l <- l + 1
      
    }
    
    forecasted_value[j] <- mean(unlist(forecast))
    Dato[j] <- date
    
    j <- j + 1
    
  }
  
  forecasted_values <- as_tibble(cbind(unlist(Dato), unlist(forecasted_value)))
  table <- cbind(municipality, forecasted_values)
  
} 

ADF_results <- foreach(municipality = municipalities, .packages = c("dplyr", "ARDL", "tidyverse", "tibble", "bootUR")) %dopar% {
  print(municipality)
}


#| label: Best model based on RMSE for model specifications

kommune_var <- "Kommune"

# Unique list of municipalities
municipalities <- unique(Ejendomspriser$Kommune)

# Placeholder for all combined forecasts across municipalities
prediction_full <- tibble()
municipality_data <- list()
i = 1


forecasted_value_rank <- list()
forecasted_value_mean <- list()


for (time in seq(start_window, end_window, by = 1)) {
  
  window_data <- municipality_data[[i]][1:(time), ] # Expanding window data for the municipality
  
  date <- municipality_data[[i]][1:time, ] |> dplyr::select(Date) |> tail(1) + 1
  
  # Store individual forecasts from each model for this window
  window_forecasts <- numeric(length(explanatory_vars))
  forecast <- list()
  l <- 1
  
  for (var in explanatory_vars) {
    
    # Define the ARDL model formula
    Growth_rate_diff <- Growth_rate_adf |> filter(municipality_name == municipality)
    var_diff <- test |> filter(municipality_name == municipality & variable_name == var)
    
    model_formula_0_0 <- reformulate(c(paste0((var))), response = dependent_var)
    model_formula_1_0 <- reformulate(c(paste0((var))), response = diff_dependent_var)
    model_formula_0_1 <- reformulate(c(paste0("diff_", (var))), response = dependent_var)
    model_formula_1_1 <- reformulate(c(paste0("diff_", (var))), response = diff_dependent_var)
    
    if(Growth_rate_diff$p_value == TRUE) {
      
      if(var_diff$p_value == TRUE) {
        
        model <- auto_ardl(model_formula_0_0, data = window_data, 
                           max_order = c(4,4),
                           starting_order = c(1,1), selection = "BIC")
        
      } else {
        
        model <- auto_ardl(model_formula_0_1, data = window_data, 
                           max_order = c(4,4),
                           starting_order = c(1,1), selection = "BIC")
        
      }
      
    } else {
      
      if(var_diff$p_value == TRUE) {
        
        model <- auto_ardl(model_formula_1_0, data = window_data, 
                           max_order = c(4,4),
                           starting_order = c(1,1), selection = "BIC")
        
      } else {
        
        model <- auto_ardl(model_formula_1_1, data = window_data, 
                           max_order = c(4,4),
                           starting_order = c(1,1), selection = "BIC")
        
      }
      
      
    }
    
    forecast_data <- municipality_data[[i]][1:(time), ]
    model_spec <- rownames_to_column(as.data.frame(model$best_order), var = "Variable")
    var_lag_list <- list(model_spec$Variable[1], model_spec$Variable[2])
    data <- add_dynamic_lags(forecast_data, var_lag_list)
    lm_model <- to_lm(model$best_model)
    forecast[l] <- predict(lm_model, data) |> tail(1)
    window_data_1 <- municipality_data[[i]][(time+1):(time+1), ] |> 
      as_tibble() |> 
      select(Growth_rate)
    rank_data <- as.tibble(unlist(forecast)) |> 
      mutate(model = row_number(),
             truth = window_data_1$Growth_rate) |> 
      group_by(model) |> 
      mutate(MSE = mean(value - truth)^2) |> 
      ungroup() |> 
      arrange(MSE) |> 
      mutate(rank = row_number(),
             weight = rank^(-1) / sum(rank)^(-1))
    
    l <- l + 1
    
  }
  
  forecasted_value_rank[j] <- sum(rank_data$weight*rank_data$value)/sum(rank_data$weight)
  
  forecasted_value_mean[j] <- mean(unlist(forecast))
  Dato[j] <- date
  
  j <- j + 1
  
}

