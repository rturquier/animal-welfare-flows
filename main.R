# Libraries ---------------------------------------------------------------
library(tidyverse)
library(glue)
library(FAOSTAT)
library(readxl)
library(readODS)
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

# Plot (adjust_ADG - ADG) vs share of imports  ------
#' This shows that countries for which the difference is largest are countries
#' that import most of their chicken consumption. Likewise, the trade adjustment
#' makes almost no difference for countries that produce almost all the chicken
#' they eat.
adjusted_ADG_df |>
  filter(year == 2024) |>
  ggplot() +
  geom_point(
    aes(
      x = share_of_imports_in_consumption,
      y = ADG_trade_difference
    ),
    alpha = 0.4,
    size = 0.6,
    color = "#D21"
  ) +
  theme_minimal()

save_fig("ADG_trade_difference-vs-share_of_imports.svg")

# Plot ADG_times_n_slaughtered vs adjusted_ADG_times_n_consumed --------
adjusted_ADG_df |>
  filter(year == 2024, country != "Iceland") |>
  arrange(desc(adjusted_ADG_times_n_consumed)) |>
  head(10) |>
  ggplot() +
  geom_segment(
    aes(
      x = ADG_times_n_slaughtered,
      xend = adjusted_ADG_times_n_consumed,
      y =  reorder(country, adjusted_ADG_times_n_consumed)
    ),
    color = "#222", linewidth = 0.6
  ) +
  geom_point(
    aes(x = ADG_times_n_slaughtered, y = country, color = "ADG"),
    shape = 19
  ) +
  geom_point(
    aes(x = adjusted_ADG_times_n_consumed, y = country, color = "adjusted_ADG"),
    shape = 17
  ) +
  scale_color_manual(
    name = NULL,
    values = c("ADG" = "#FFCC22", "adjusted_ADG" = "#EE9922")
  ) +
  labs(
    x = glue("ADG  (g / day) × number of chickens (killed or consumed) in 2024"),
    y = NULL,
    title = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

save_fig("top 10 ADG times number of chickens.png")

# Same thing with number of chickens --------
adjusted_ADG_df |>
  filter(year == 2024, country != "Iceland") |>
  arrange(desc(adjusted_ADG_times_n_consumed)) |>
  head(10) |>
  ggplot() +
  geom_segment(
    aes(
      x = n_slaughtered,
      xend = n_consumed,
      y =  reorder(country, n_consumed)
    ),
    color = "#222", linewidth = 0.6
  ) +
  geom_point(
    aes(x = n_slaughtered, y = country, color = "n_slaughtered"),
    shape = 19
  ) +
  geom_point(
    aes(x = n_consumed, y = country, color = "n_consumed"),
    shape = 17
  ) +
  scale_color_manual(
    name = NULL,
    values = c("n_slaughtered" = "#FFCC22", "n_consumed" = "#EE9922")
  ) +
  labs(
    x = glue("Number of chickens killed and consumed in 2024"),
    y = NULL,
    title = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

save_fig("top 10 number of chickens killed and consumed.png")

# Prepare EU laying hens housing system data ------------------------------
EU_hen_path <- "data/raw/laying-hens-by-way-of-keeping-2011-2025_en.xlsx"
download_EU_hen_data(EU_hen_path)
EU_hen_df <- prepare_EU_hen_df(EU_hen_path)

# Plot shares in a given country ----------------------------------------------
EU_hen_df |> plot_housing_shares("Czechia")

# Map housing shares in a given year --------------------------------------
map_hen_housing(EU_hen_df, year=2024)

# Compare imports and share of cages --------------------------------------
EU_egg_trade_df <- prepare_EU_egg_trade_df(production_df, FAO_trade_matrix)

# Plot --------------------------------------------------------------------
EU_egg_trade_df |> plot_cage_and_import_shares(country = "Austria")

# Read global hen housing data --------------------------------------------
WFI_hens_df <- prepare_WFI_hens_df("Maps_Full Data_data.csv")
manual_cage_shares_df <- read_csv("data/manual_cage_shares.csv")

UK_hen_path <- "data/raw/UK_hen_cage_shares.ods"
download_UK_hen_data(UK_hen_path)
UK_hen_df <- prepare_UK_hen_data(UK_hen_path)

OWID_hen_path <- "data/raw/OWID_hen_data.csv"
download_OWID_hen_data(OWID_hen_path)
OWID_hen_df <- prepare_OWID_hen_data(OWID_hen_path)

# Get largest EU trade partners -----
top_EU_partners <- FAO_trade_matrix |>
  filter(
    item == "Hen eggs in shell, fresh",
    country %in% EU_27,
  ) |>
  group_by(year, partner) |>
  summarise(imports = sum(imports), exports = sum(exports)) |>
  filter(!(partner) %in% c(EU_27, "Belgium-Luxembourg")) |>
  arrange(desc(imports)) |>
  top_n(10, imports)

# Extract cage data from CIWF 2020 report ----------------------------------
ciwf_cage_df <- extract_ciwf_cage_data()

# Eurostat data on pig housing -----------------------------------------------
library(eurostat)

pig_data_path <- "data/raw/pig_housing.csv"
download_eurostat_pig_data(pig_data_path)
pig_data <- pig_data_path |> read_csv()

pig_df <- pig_data |>
  label_eurostat(fix_duplicated = T)
