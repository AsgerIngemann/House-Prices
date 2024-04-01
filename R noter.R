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

