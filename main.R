# Libraries ---------------------------------------------------------------
library(tidyverse)
library(glue)
library(FAOSTAT)
library(readxl)
library(countrycode)
library(pdftools)
library(maps)
library(seas)
library(archive)
library(zoo)

library(duckdb)
library(dbplyr)

# Prevent MASS from masking common dplyr functions
library(conflicted)
conflicts_prefer(dplyr::select(), dplyr::filter())

# Functions --------------------------------------------------------------
source("functions.R")

# FAOSTAT login ----------------------------------------------------------
# Go to https://www.fao.org/faostat/en/#developer-portal to get credentials.
faostat_login()

# Download FAO data -------------------------------------------------------
production_data_path <- "data/raw/production.rds"
FAO_trade_data_path <- "data/raw/trade.rds"
FAO_trade_matrix_path <- "data/raw/FAO_trade_matrix.rds"

download_production_data(production_data_path)
download_FAO_trade_data(FAO_trade_data_path)
download_FAO_trade_matrix(FAO_trade_matrix_path)

# Read downloaded data ----------------------------------------------------
raw_production_df <- read_rds(production_data_path)
raw_FAO_trade_df <- read_rds(FAO_trade_data_path)
raw_FAO_trade_matrix <- read_rds(FAO_trade_matrix_path)

# Download and prepare BACI data ------------------------------------------
download_and_prepare_BACI_data("data/raw/BACI.rds")
BACI_df <- read_rds("data/raw/BACI.rds")

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
production_df <- prepare_production_df(raw_production_df, scope)

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
  select(country, animal, year, slaughter_rate)

# Wrangle trade data ------------------------------------------------------
FAO_trade_df <- prepare_FAO_trade_df(raw_FAO_trade_df)
FAO_trade_matrix <- prepare_FAO_trade_matrix(raw_FAO_trade_matrix)

# Plot evolution of global meat flows -------------------------------------
plot_global_meat_imports(FAO_trade_df)
save_fig("global_meat_imports.svg")

# Map net exports --------------------------------------------------------
FAO_trade_df |> map_net_exports()
save_fig("net_chicken_meat_exports.svg")

# Map Average Daily weight Gain (ADG) -------------------------------------
production_df |> map_ADG(year = 2024)
save_fig("ADG.svg")

# Adjust ADG for trade flows ----------------------------------------------
broiler_production_df <- production_df |>
  filter(animal == "broilers") |>
  select(-animal, -n_eggs)

adjusted_ADG_df <- FAO_trade_matrix |>
  filter(item == "Meat of chickens, fresh or chilled") |>
  select(-c(item)) |>
  inner_join(
    broiler_production_df,
    by = c("country", "year"),
    relationship = "many-to-one"
  ) |>
  inner_join(
    broiler_production_df,
    by = c("partner" = "country", "year"),
    suffix = c(".country", ".partner")
  ) |>
  rename(
    imports_kg = imports,
    exports_kg = exports,
  ) |>
  mutate(
    imports_n_animals = imports_kg / weight_per_animal.partner,
    exports_n_animals = exports_kg / weight_per_animal.country,
  ) |>
  group_by(country, year) |>
  summarise(
    # number of animals killed for consumption
    #   = number of animals killed in country + imports - exports
    consumption_n_animals = unique(n_slaughtered.country)
                            + sum(imports_n_animals)
                            - sum(exports_n_animals)
  )

# Quick plot of French egg importations ---------------------------------
library("slider")

france_df <- FAO_trade_matrix |>
  filter(
    country == "France",
    animal == "hens",
    element == "import_quantity"
  ) |>
  ungroup() |>
  group_by(year) |>
  summarise(quantity = sum(quantity), unit = unique(unit)) |>
  ungroup() |>
  left_join(
    filter(production_df, animal == "hens", country == "France"),
    join_by(year == year)
  ) |>
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
