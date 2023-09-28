library(tidyverse)
library(readxl)
library(lubridate)



lpg_5kg  <- read_xlsx("data/raw-data/gas.xlsx", sheet = "5kg")
lpg_12kg <- read_xlsx("data/raw-data/gas.xlsx", sheet = "12kg")



north_central <- c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "Abuja")
north_east <- c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe")
north_west <- c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara")
south_east <- c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo")
south_south <- c("Akwa Ibom", "Bayelsa", "Cross River", "Edo", "Rivers", "Delta")
south_west <- c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")



# lpg_5kg |> 
#   mutate(`ITEM LABEL` = "Small Cylinder (5KG)") |>
#   pivot_longer(
#     cols = -c(`STATE LABEL`, `ITEM LABEL`),
#     names_to = "date",
#     values_to = "average_price",
#     values_transform = list(average_price = as.numeric)
#   ) |>
#   mutate(
#     date = as.numeric(date) |> as.Date(origin = "1899-12-30"),
#     geo_zone = case_when(
#       `STATE LABEL` %in% north_central ~ "North Central",
#       `STATE LABEL` %in% north_east    ~ "North East",
#       `STATE LABEL` %in% north_west    ~ "North West",
#       `STATE LABEL` %in% south_east    ~ "South East",
#       `STATE LABEL` %in% south_south   ~ "South South",
#       `STATE LABEL` %in% south_west    ~ "South West",
#       .default = `STATE LABEL`
#     )
#   ) |>
#   rename(state_and_capital = `STATE LABEL`, item = `ITEM LABEL`) |>
#   relocate(geo_zone, .after = state_and_capital) |>
#   filter(between(year(date), 2020, 2023)) 


clean_lpg_data <- function(item, size) {
  
  label <- ifelse(
    size == "small", 
    "Small Cylinder (5KG)",
    "Medium Cylinder (10KG or 12.5KG)"
  )
  
  item |> 
    mutate(`ITEM LABEL` = label) |>
    pivot_longer(
      cols = -c(`STATE LABEL`, `ITEM LABEL`),
      names_to = "date",
      values_to = "average_price",
      values_transform = list(average_price = as.numeric)
    ) |>
    mutate(
      date = as.numeric(date) |> as.Date(origin = "1899-12-30"),
      `STATE LABEL` = if_else(`STATE LABEL` == "Nassarawa", "Nasarawa", `STATE LABEL`),
      geo_zone = case_when(
        `STATE LABEL` %in% north_central ~ "North Central",
        `STATE LABEL` %in% north_east    ~ "North East",
        `STATE LABEL` %in% north_west    ~ "North West",
        `STATE LABEL` %in% south_east    ~ "South East",
        `STATE LABEL` %in% south_south   ~ "South South",
        `STATE LABEL` %in% south_west    ~ "South West",
        .default = `STATE LABEL`
      )
    ) |>
    rename(state_and_capital = `STATE LABEL`, item = `ITEM LABEL`) |>
    relocate(geo_zone, .after = state_and_capital) |>
    filter(between(year(date), 2019, 2023)) 
  
}


lpg <- clean_lpg_data(lpg_5kg, "small") |>
  bind_rows(clean_lpg_data(lpg_12kg, "medium"))


write_csv(lpg, "data/clean_data/lpg.csv")
