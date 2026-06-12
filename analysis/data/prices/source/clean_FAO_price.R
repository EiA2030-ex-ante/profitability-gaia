library(tidyverse)

fao_price_data_0 <- read_csv("FAO_price_Tue_Oct_28_2025.csv")

fao_price_data_1 <- fao_price_data_0 %>%
  select(-1)|>
  pivot_longer(cols = -c(Date),
               names_to = "vars",
               values_to = "Price_USD") |>
  separate(vars, into = c("country", "price_type", "market", "crop", "unit"), sep = ",")|>
  # extract year from Date , dontuse lubridate from this format 09/01/2025
  mutate(year = as.integer(substr(Date, 7, 10))) %>% # group by country, crop, year and calculate average price
  group_by(country, crop, year) %>%
  summarise(avg_price = mean(Price_USD, na.rm = TRUE), .groups = "drop") |>
  # price per ton
  mutate(price_per_ton =  avg_price * 1000) 
