library("tidyverse")
library("FAOSTAT")


download_production_data <- function() {
  get_faostat_bulk("QCL") |> saveRDS("data/raw/production.rds")
}
download_trade_data <- function() {
  get_faostat_bulk("TM") |> saveRDS("data/raw/trade.rds")
}

download_production_data()
download_trade_data()
