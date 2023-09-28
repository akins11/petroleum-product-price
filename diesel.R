library(tidyverse)
library(readxl)
library(lubridate)


diesel <- read_xlsx("data/raw-data/diesel.xlsx")


north_central <- c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "Abuja")
north_east <- c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe")
north_west <- c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara")
south_east <- c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo")
south_south <- c("Akwa Ibom", "Bayelsa", "Cross River", "Edo", "Rivers", "Delta")
south_west <- c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")


# Diesel oil
ds <- diesel |>
  select(-ITEMLABELS) |>
  pivot_longer(
    cols = -STATELABEL,
    names_to = "date",
    values_to = "average_price",
    values_transform = list(average_price = as.numeric)
  ) |>
  mutate(
    date = as.numeric(date) |> as.Date(origin = "1899-12-30"),
    geo_zone = case_when(
      STATELABEL %in% north_central ~ "North Central",
      STATELABEL %in% north_east    ~ "North East",
      STATELABEL %in% north_west    ~ "North West",
      STATELABEL %in% south_east    ~ "South East",
      STATELABEL %in% south_south   ~ "South South",
      STATELABEL %in% south_west    ~ "South West",
      .default = STATELABEL
    )
  ) |>
  rename(state_and_capital = STATELABEL) |>
  relocate(geo_zone, .after = state_and_capital) |>
  filter(between(year(date), 2019, 2023))


ds |> distinct(state_and_capital) |> print(n = 37)
ds |> distinct(geo_zone)


diesel |> distinct(STATELABEL) |> print(n = 37)


write_csv(ds, "data/clean_data/diesel.csv")
