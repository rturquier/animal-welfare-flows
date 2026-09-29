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
adjusted_ADG_df |> plot_adjusted_ADG(year = 2024)
save_fig("adjusted_ADG_2024.png", height = 2000)

# Plot both ADGs across time for a given country --------------------------
adjusted_ADG_df |> plot_ADG_evolution(country = "EU_27")

# Prepare EU laying hens housing system data ------------------------------
hen_housing_path <- "data/raw/laying-hens-by-way-of-keeping-2011-2025_en.xlsx"
download_hen_housing_data(hen_housing_path)
hen_housing_df <- prepare_hen_housing_df(hen_housing_path)

# Plot shares in a given country ----------------------------------------------
hen_housing_df |> plot_housing_shares("Poland")

# Map housing shares in a given year --------------------------------------
map_hen_housing(hen_housing_df, year=2024)

# Compare imports and share of cages --------------------------------------
EU_egg_trade_df <- prepare_EU_egg_trade_df(production_df, FAO_trade_matrix)

# Plot --------------------------------------------------------------------
EU_egg_trade_df |> plot_cage_and_import_shares(country = "Germany")

# Extract cage data from CIWF 2020 report ----------------------------------
ciwf_cage_df <- extract_ciwf_cage_data()

# Eurostat data on pig housing -----------------------------------------------
library(eurostat)

pig_housing_data_path <- "data/raw/pig_housing.csv"
download_eurostat_pig_data(pig_housing_data_path)
pig_housing_data <- pig_housing_data_path |> read_csv()

pig_housing_df <- pig_housing_data |>
  label_eurostat(fix_duplicated = T)
