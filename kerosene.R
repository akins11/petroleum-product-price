library(tidyverse)
library(readxl)
library(lubridate)


kerosene_liter  <- read_xlsx("data/raw-data/kerosene.xlsx", sheet = "liter")
kerosene_gallon <- read_xlsx("data/raw-data/kerosene.xlsx", sheet = "gallon")


north_central <- c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "Abuja")
north_east <- c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe")
north_west <- c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara")
south_east <- c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo")
south_south <- c("Akwa Ibom", "Bayelsa", "Cross River", "Edo", "Rivers", "Delta")
south_west <- c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")


# One gallon ==> 4.5 Liters

kerosene_gallon |>
  select(-ITEMLABELS) |>
  pivot_longer(
    cols = -c(STATELABEL, `Unit of Measure`),
    names_to = "date",
    values_to = "average_price",
    values_transform = list(average_price = as.numeric)
  ) |> distinct(date) |> print(n = 96)
  mutate(
    date = as.numeric(date) |> as.Date(origin = "1899-12-30"),
    STATELABEL = if_else(STATELABEL == "Nassarawa", "Nasarawa", STATELABEL),
    geo_zone = case_when(
      STATELABEL %in% north_central ~ "North Central",
      STATELABEL %in% north_east    ~ "North East",
      STATELABEL %in% north_west    ~ "North West",
      STATELABEL %in% south_east    ~ "South East",
      STATELABEL %in% south_south   ~ "South South",
      STATELABEL %in% south_west    ~ "South West",
      .default = STATELABEL
    )
  ) #|>
  rename(state_and_capital = STATELABEL, unit_of_measure = `Unit of Measure`) |>
  relocate(geo_zone, .after = state_and_capital) |>
  filter(between(year(date), 2020, 2023)) 

clean_kerosene_data <- function(unit_data, unit) {
  
  unit_label <- ifelse(
    unit == "liter",
    "1 Liter",
    "1 Gallon"
  )
  
  unit_data |>
    select(-ITEMLABELS) |>
    mutate(`Unit of Measure` = unit_label) |>
    pivot_longer(
      cols = -c(STATELABEL, `Unit of Measure`),
      names_to = "date",
      values_to = "average_price",
      values_transform = list(average_price = as.numeric)
    ) |>
    mutate(
      date = as.numeric(date) |> as.Date(origin = "1899-12-30"),
      STATELABEL = if_else(STATELABEL == "Nassarawa", "Nasarawa", STATELABEL),
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
    rename(state_and_capital = STATELABEL, unit_of_measure = `Unit of Measure`) |>
    relocate(geo_zone, .after = state_and_capital) |>
    filter(between(year(date), 2019, 2023)) 
  
}


ks <- clean_kerosene_data(kerosene_liter, "liter") |>
  bind_rows(clean_kerosene_data(kerosene_gallon, "gallon"))


ks |> distinct(geo_zone)


write_csv(ks, "data/clean_data/kerosene.csv")
