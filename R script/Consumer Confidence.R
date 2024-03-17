Consumer_Confidence_raw <- read_excel("Data/Consumer Confidence - National Monthly.xlsx", 
                                                   skip = 2)

Consumer_Confidence <- Consumer_Confidence_raw |> 
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

Consumer_Confidence |> 
  autoplot(diff_Consumer_Confidence)

## Looks like white noise