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
if (!raw_data_is_downloaded) {
  download_production_data()
  download_trade_data()
}

raw_production_df <- read_rds("data/raw/production.rds")
raw_trade_df <- read_rds("data/raw/trade.rds")
