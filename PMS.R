library(tidyverse)
library(readxl)
library(lubridate)


petrol <- read_xlsx("data/raw-data/petrol.xlsx")


north_central <- c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "Abuja")
north_east <- c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe")
north_west <- c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara")
south_east <- c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo")
south_south <- c("Akwa Ibom", "Bayelsa", "Cross River", "Edo", "Rivers", "Delta")
south_west <- c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")

pt <- petrol |>
  pivot_longer(
    cols = -State,
    names_to = "date",
    values_to = "average_price",
    values_transform = list(average_price = as.numeric)
  ) |>
  mutate(
    date = as.numeric(date) |> as.Date(origin = "1899-12-30"),
    State = if_else(State == "Nassarawa", "Nasarawa", State),
    geo_zone = case_when(
      State %in% north_central ~ "North Central",
      State %in% north_east    ~ "North East",
      State %in% north_west    ~ "North West",
      State %in% south_east    ~ "South East",
      State %in% south_south   ~ "South South",
      State %in% south_west    ~ "South West",
      .default = State
    )
  ) |>
  rename(state_and_capital = State) |>
  relocate(geo_zone, .after = state_and_capital) |>
  filter(between(year(date), 2019, 2023))



pt |> distinct(state_and_capital)
pt |> distinct(geo_zone)

# check NA
sapply(pt, \(x) sum(is.na(x)))


pt |>
  filter(state_and_capital == "Abia")

write_csv(pt, "data/clean_data/petrol.csv")
