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
adjusted_ADG_df <- adjust_ADG_for_trade_flows(production_df, FAO_trade_matrix)

# Plot the difference -----------------------------------------------------
adjusted_ADG_df |> plot_adjusted_ADG(year = 2023)
save_fig("adjusted_ADG_2023.png", height = 2000)

# Prepare EU laying hens housing system data ------------------------------
hen_housing_path <- "data/raw/laying-hens-by-way-of-keeping-2011-2025_en.xlsx"
download_hen_housing_data(hen_housing_path)
hen_housing_df <- prepare_hen_housing_df(hen_housing_path)

# Plot shares in a given country ----------------------------------------------
hen_housing_df |> plot_housing_shares("Belgium")

# Map housing shares in a given year --------------------------------------
map_hen_housing(hen_housing_df, year=2024)

# Compare imports and share of cages --------------------------------------
egg_production_df <- production_df |>
  filter(animal == "hens") |>
  select(country, year, n_eggs, production_kg)

EU_egg_trade_df <- FAO_trade_matrix |>
  filter(
    item |> str_detect("(E|e)ggs"),
    country %in% EU_27
  ) |>
  group_by(country, year, item) |>
  summarise(
    imports = sum(imports),
    exports = sum(exports),
    unit = unique(unit),
    .groups = "drop"
  ) |>
  left_join(hen_housing_df, by = c("country", "year")) |>
  left_join(egg_production_df, by = c("country", "year")) |>
  rename(production = n_eggs) |>
  relocate(production, production_kg, .before = imports) |>
  group_by(country, item) |>
  mutate(
    production = if_else(item == "Hen eggs in shell, fresh", production, NA),
    apparent_consumption = production - exports + imports,
    imports_share = imports / apparent_consumption,
    imports_share_base_100_2011 =
      100 * imports_share / imports_share[year == 2011],
    share_cage = share_enriched + replace_na(share_battery, 0),
    share_cage_base_100_2011 = 100 * share_cage / share_cage[year == 2011],
  )

EU_egg_trade_df |>
  filter(
    year >= 2011,
    item == "Hen eggs in shell, fresh",
    country == "Denmark"
  ) |>
  ggplot(aes(x = year)) +
    geom_line(aes(y = share_cage_base_100_2011, color = "cages")) +
    geom_point(aes(y = share_cage_base_100_2011, color = "cages")) +
    geom_line(aes(y = imports_share_base_100_2011, color = "imports")) +
    geom_point(aes(y = imports_share_base_100_2011, color = "imports")) +
    scale_color_manual(
      name = NULL,
      values = c("cages" = "#EE9922", "imports" = "#FFCC22")
    ) +
    labs(
      y = "share",
      title = "Share of cage egg production and share of imports in Denmark"
    ) +
    theme_minimal()




# Extract cage data from CIWF 2020 report ----------------------------------
ciwf_cage_df <- extract_ciwf_cage_data()

# Eurostat data on pig housing -----------------------------------------------
library(eurostat)

pig_housing_data_path <- "data/raw/pig_housing.csv"
download_eurostat_pig_data(pig_housing_data_path)
pig_housing_data <- pig_housing_data_path |> read_csv()

pig_housing_df <- pig_housing_data |>
  label_eurostat(fix_duplicated = T)
