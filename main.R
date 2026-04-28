# Libraries ---------------------------------------------------------------
library("tidyverse")
library("glue")
library("FAOSTAT")
library("readxl")
library("countrycode")
library("pdftools")

# Functions --------------------------------------------------------------
source("functions.R")

# Download FAO data -------------------------------------------------------
production_data_path <- "data/raw/production.rds"
trade_data_path <- "data/raw/trade.rds"

# Go to https://www.fao.org/faostat/en/#developer-portal to get FAO credentials.
# You might need to run `faostat_login()` to connect.
download_production_data(production_data_path)
download_trade_data(trade_data_path)

# Read downloaded data ----------------------------------------------------
raw_production_df <- read_rds(production_data_path)
raw_trade_df <- read_rds(trade_data_path)

# Scope -------------------------------------------------------------------
scope <- list(
  'items' = c(
    "Chickens",
    "Meat of chickens, fresh or chilled",
    "Meat of chickens, frozen",
    "Hen eggs in shell, fresh",
    "Meat of pig with the bone, fresh or chilled",
    "Swine / pigs"
  ),
  'elements' = c(
    "producing_animals_slaughtered", "stocks", "laying", "production"
  )
)

# Wrangle production data -------------------------------------------------
production_df <- raw_production_df |>
  rename(quantity = value) |>
  filter(
    item %in% scope$items,
    element %in% scope$elements
  ) |>
  convert_FAO_units() |>
  mutate(
    animal = case_when(
      item |> str_detect("C|chickens") ~ "broilers",
      item == "Hen eggs in shell, fresh" ~ "hens",
      item |> str_detect("pig") ~ "pigs",
      .default = "other"
    ),
    variable_name = case_when(
      element == "producing_animals_slaughtered" ~ "n_slaughtered",
      element %in% c("stocks", "laying") ~ "n_animals",
      item |> str_detect("eggs") & unit == "number" ~ "n_eggs",
      element == "production" ~ "production_{unit}" |> glue()
    )
  ) |>
  select(area, year, animal, variable_name, quantity) |>
  pivot_wider(
    names_from = variable_name,
    values_from = quantity
  ) |>
  arrange(area, year) |>
  mutate(
    slaughter_rate = n_slaughtered / n_animals,
    weight_per_animal = production_kg / n_slaughtered
  )

# Quick scatter plot of the two proxies ------------------------------------
animal_to_plot <- "broilers"
year_to_plot <- 2023

production_df |>
  filter(animal == animal_to_plot, year == year_to_plot) |>
  ggplot(aes(x = slaughter_rate, y = weight_per_animal)) +
    geom_point() +
    labs(title="{animal_to_plot}, {year_to_plot}" |> glue())

# Look for outliers
production_df |>
  filter(slaughter_rate > 10, year > 2020) |>
  select(area, animal, year, slaughter_rate)

# Wrangle trade data ------------------------------------------------------
trade_df <- raw_trade_df |> prepare_trade_df()

third_party_aggregated_trade_df <- trade_df |>
  group_by(partner, year, element, animal, unit) |>
  summarise(quantity = sum(quantity), .groups = "drop") |>
  pivot_wider(
    names_from = element,
    values_from = quantity
  ) |>
  rename(
   country = partner,
   export = import_quantity,
   import = export_quantity
  )

self_reported_aggregated_trade_df <- trade_df |>
  group_by(country, year, element, animal, unit) |>
  summarise(quantity = sum(quantity), .groups = "drop") |>
  pivot_wider(
    names_from = element,
    values_from = quantity
  ) |>
  rename(
    import = import_quantity,
    export = export_quantity
  )

aggregated_trade_df <-
  full_join(
    self_reported_aggregated_trade_df,
    third_party_aggregated_trade_df,
    by = c("country", "year", "animal", "unit"),
    suffix = c("_self_reported", "_third_party")
  ) |>
  mutate(
    import = consolidate_trade_flow(import_self_reported, import_third_party),
    export = consolidate_trade_flow(export_self_reported, export_third_party)
  )

# Quick plot of French egg importations ------------------------------
library("slider")

france_df <- trade_df |>
  filter(
    country == "France",
    animal == "hens",
    element == "import"
  ) |>
  ungroup() |>
  group_by(year, item) |>
  summarise(quantity = sum(quantity), unit = unique(unit)) |>
  ungroup() |>
  left_join(filter(production_df, animal == "hens", area == "France"), join_by(year == year)) |>
  mutate(
    kg_per_egg = production_kg / n_eggs,
    kg_per_egg_roll_mean = slide(kg_per_egg, mean, .before = 10) |> as.double(),
    kg_per_egg_roll_mean = replace_na(last(kg_per_egg_roll_mean, na_rm = T)),
    kg_per_egg = coalesce(kg_per_egg, kg_per_egg_roll_mean),
    production_kg = coalesce(production_kg, n_eggs * kg_per_egg),
    imported_share = quantity / production_kg
  )

france_df |> ggplot(aes(x = year, y = imported_share)) + geom_line()

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
  distinct(country, year, .keep_all = TRUE) |>
  arrange(country, year) |>
  relocate(n_free_range, .after = n_barn) |>
  relocate(share_free_range, .after = share_barn) |>
  mutate(across(matches("year|n_.+"), as.integer))

# Replace Belgium and Poland 2014 with NAs ----
# For Belgium and Poland, 2014 is nearly identical to 2013, but with `barn` and
# `free_range` swapped. This is probably the result of a human mistake.
be_2014_row <- which(hen_housing_df$country == "Belgium"
                     & hen_housing_df$year == 2014)
pl_2014_row <- which(hen_housing_df$country == "Poland"
                     & hen_housing_df$year == 2014)
all_columns_except_country_and_year <- hen_housing_df |> colnames() |> tail(-2)
hen_housing_df[be_2014_row, all_columns_except_country_and_year] <- NA
hen_housing_df[pl_2014_row, all_columns_except_country_and_year] <- NA

# Plot shares in a given country ----------------------------------------------
hen_housing_df |> plot_housing_shares("Belgium")

# Extract cage data from CIWF 2020 report ----------------------------------
ciwf_cage_df <- extract_ciwf_cage_data()

# Eurostat data on pig housing -----------------------------------------------
library(eurostat)

pig_housing_data_path <- "data/raw/pig_housing.csv"
download_eurostat_pig_data(pig_housing_data_path)
pig_housing_data <- pig_housing_data_path |> read_csv()

pig_housing_df <- pig_housing_data |>
  label_eurostat(fix_duplicated = T)
