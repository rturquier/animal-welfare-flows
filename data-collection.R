# Libraries ---------------------------------------------------------------
library("tidyverse")
library("FAOSTAT")
library("readxl")
library("countrycode")

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

# Prepare EU laying hens housing system data ------------------------------
egg_dashboard_df <- read_csv("data/EU_eggs_dashboards.csv") |>
  mutate(country = countrycode(country, "eurostat", "country.name.en")) |>
  mutate(across(starts_with("percentage"),
                ~ . / 100,
                .names = "{str_replace(.col, 'percentage', 'share')}"),
        .keep = "unused") |>
  mutate(across(
    matches("share_(enriched|barn|free_range|organic)"),
    ~ round(. * n_laying_hens),
    .names = "{str_replace(.col, 'share', 'n')}"
  ))


hen_housing_2011_2021_numbers <- read_excel(
  "data/raw/laying-hens-keeping-2011-2020_en.xlsx",
  range = "A4:H300",
  col_names = c(
    "country",
    "year",
    "n_battery",
    "n_enriched",
    "n_free_range",
    "n_barn",
    "n_organic",
    "n_laying_hens"
  )
)
hen_housing_2011_2021_shares <- read_excel(
  "data/raw/laying-hens-keeping-2011-2020_en.xlsx",
  range = "L4:P300",
  col_names = c(
    "share_battery",
    "share_enriched",
    "share_free_range",
    "share_barn",
    "share_organic"
  )
)

hen_housing_df <- hen_housing_2011_2021_numbers |>
  bind_cols(hen_housing_2011_2021_shares) |>
  fill(country) |>
  mutate(country = countryname(country)) |>
  bind_rows(egg_dashboard_df) |>
  arrange(country, year)


# Swap barn and free range for Belgium 2014 to correct a likely mistake ----
be_2014_row <- which(hen_housing_df$country == "Belgium"
                     & hen_housing_df$year == 2014)

be_2014_n_free_range <- hen_housing_df[[be_2014_row, "n_free_range"]]
be_2014_share_free_range <- hen_housing_df[[be_2014_row, "share_free_range"]]
be_2014_n_barn <- hen_housing_df[[be_2014_row, "n_barn"]]
be_2014_share_barn <- hen_housing_df[[be_2014_row, "share_barn"]]

hen_housing_df[[be_2014_row, "n_free_range"]] <- be_2014_n_barn
hen_housing_df[[be_2014_row, "n_barn"]] <- be_2014_n_free_range

hen_housing_df[[be_2014_row, "share_free_range"]] <- be_2014_share_barn
hen_housing_df[[be_2014_row, "share_barn"]] <- be_2014_share_free_range
