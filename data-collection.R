# Libraries ---------------------------------------------------------------
library("tidyverse")
library("FAOSTAT")

# Download FAO data -------------------------------------------------------
download_production_data <- function() {
  get_faostat_bulk("QCL") |> saveRDS("data/raw/production.rds")
}
download_trade_data <- function() {
  get_faostat_bulk("TM") |> saveRDS("data/raw/trade.rds")
}

raw_data_is_downloaded <- file.exists("data/raw/production.rds",
                                      "data/raw/trade.rds")
if (!all(raw_data_is_downloaded)) {
  download_production_data()
  download_trade_data()
}

# Read downloaded data ----------------------------------------------------
raw_production_df <- read_rds("data/raw/production.rds")
raw_trade_df <- read_rds("data/raw/trade.rds")


# Scope -------------------------------------------------------------------
# For now, restrict scope to chickens in France and Germany, in 2007
scope <- list(
  'countries' = c("France", "Germany"),
  'years' = 2007,
  'items' = c(
    "Chickens",
    "Meat of chickens, fresh or chilled",
    "Meat of chickens, frozen",
    "Hen eggs in shell, fresh"
  ),
  'elements' = c(
    "producing_animals_slaughtered", "stocks", "laying", "production"
  )
)

# Wrangle production data -------------------------------------------------
production_df <- raw_production_df |>
  filter(
    area %in% scope$countries,
    year %in% scope$years,
    item %in% scope$items,
    element %in% scope$elements
  ) |>
  mutate(
    conversion_factor = case_match(unit,
                                   "1000 An" ~ 1000,
                                   "1000 No" ~ 1000,
                                   .default = 1),
    value = value * conversion_factor,
    unit = case_match(unit,
                      "1000 An" ~ "animals",
                      "1000 No" ~ "number",
                      .default = unit)
  ) |>
  select(area, year, item, element, value, unit, flag, note) |>
  arrange(area, year)
