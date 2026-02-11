library("tidyverse")
library("FAOSTAT")


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

raw_production_df <- read_rds("data/raw/production.rds")
raw_trade_df <- read_rds("data/raw/trade.rds")

scope <- list(
  'countries' = c("France", "Germany"),
  'years' = 2007:2008,
  'items' = c("Meat of chickens, fresh or chilled")
)

production_df <- raw_production_df |>
  filter(
    area %in% scope$countries,
    year %in% scope$years,
    item %in% scope$items
  ) |>
  select(area, year, item, element, value, unit) |>
  arrange(area, year)

