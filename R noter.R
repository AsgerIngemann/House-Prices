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

availability_long |> 
  filter(!grepl("^log|^diff", variable)) |> 
  ggplot(aes(x = Date, y = variable, fill = availability)) +
  geom_tile() +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey")) +
  labs(title = "Data Availability Over Time by Municipality", x = "Date", y = "Variable", fill = "Available") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x = element_text(angle = 0))


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
