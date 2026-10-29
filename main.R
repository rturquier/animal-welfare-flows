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
# (Not needed for now)
# download_and_prepare_BACI_data("data/raw/BACI.rds")
# BACI_df <- read_rds("data/raw/BACI.rds")

# Wrangle production data -------------------------------------------------
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
save_fig("ADG_map.svg")

# Adjust ADG for trade flows ----------------------------------------------
adjusted_ADG_df <- adjust_ADG_for_trade_flows(production_df, FAO_trade_matrix)

# Plot the difference -----------------------------------------------------
adjusted_ADG_df |> plot_ADG_by_country(year = 2024, only_top_n = 10)
save_fig("adjusted_ADG_2024.png")

# Compare to ADG of average imported chicken ------------------------------
adjusted_ADG_df |>
  plot_ADG_by_country(year = 2024, only_top_n = 30, compare_to=partner_ADG)

# Scatterplot ADG by country ---------------------------------------------
adjusted_ADG_df |>
  scatterplot_ADG_by_country(year = 2024, compare_to = partner_ADG)

save_fig("ADG_vs_partner_ADG_2024.svg", width = 349, height = 333)

adjusted_ADG_df |>
  scatterplot_ADG_by_country(year = 2024, compare_to = adjusted_ADG)

save_fig("ADG_vs_adjusted_ADG_2024.svg", width = 349, height = 333)

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
country = "France"
EU_hen_df |> plot_housing_shares(country)
save_fig(glue("hen_housing_shares_{country}.svg"))

# Map housing shares in a given year --------------------------------------
map_hen_housing(hen_df, year=2025)
save_fig("hen_housing_map.svg")

# Read global hen housing data --------------------------------------------
hen_df <- prepare_hen_df()

# Plot total numbers of hens by system in the EU --------------------------
hen_df |> prepare_total_EU_hen_df() |> plot_total_EU_housing_shares()
save_fig("total_EU_housing_shares.svg")

# Compare imports and share of cages --------------------------------------
egg_trade_df <- prepare_egg_trade_df(
  production_df,
  FAO_trade_matrix,
  hen_df
)

# Germany is the largest egg importer in the world ------------------------
FAO_trade_matrix |>
  group_by(country, year) |>
  summarise(imports = sum(imports)) |>
  group_by(year) |>
  top_n(1) |>
  arrange(desc(year))

# Plot production, imports and exports by housing system -----------------
country <- "Lithuania"
year <- 2024
egg_trade_df |> plot_apparent_consumption_by_system(country, year)
save_fig(glue("apparent_consumption_by_system_{country}_{year}.svg"))

# Plot both Germany and Poland --------------------------------------------
any_housing_type <- "(battery|enriched|barn|free_range|organic)"

egg_trade_df |>
  filter(
    item == "Hen eggs in shell, fresh",
    country %in% c("Poland", "Germany"),
    year == .env$year
  ) |>
  mutate(exports = -exports) |>  # To show exports below x-axis
  pivot_longer(
    c(imports, exports, production_not_exported),
    names_to = "variable_name",
    values_to = "n_eggs"
  ) |>
  mutate(
    variable_name = factor(variable_name, levels = c("imports",
                                                     "production_not_exported",
                                                     "exports")  ),
    country = factor(country, levels = c("Poland", "Germany"))
  ) |>
  ggplot(aes(fill=variable_name, y=n_eggs, x=housing_type)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(
    name = "Eggs:",
    values = c("#E92", "#0F5E89", "#1585C1"),
    labels = c("imported", "produced\nminus exported", "exported"),
  ) +
  scale_x_discrete(
    labels = rev(hen.system.names) |> str_replace(" cages", "\ncages")
  ) +
  scale_y_continuous(
    labels = \(x) abs(x / 10^9),  # convert to billions of eggs
  ) +
  facet_wrap(~country) +
  labs(
    x = NULL,
    y = NULL,
    subtitle = "Billions of eggs"
  ) +
  theme_minimal() +
  theme(plot.title.position = "plot")

save_fig("apparent_consumption_Germany_Poland_2024.svg", width = 810)

# Convert eggs to pain ----------------------------------------------------
egg_pain_df <- prepare_egg_pain_df(egg_trade_df)

# Plot imported pain ------------------------------------------------------
country <- "Germany"
year <- 2024
egg_pain_df |> plot_imported_pain_by_level(country, year)
save_fig(glue("imported_pain_by_level_{country}_{year}.svg"))

# Aggregate pain ----------------------------------------------------------
pain_weights <- c(10^(-1), 1, 10^1, 10^2)
equivalent_pain_df <- egg_pain_df |> aggregate_pain(pain_weights)

# Plot aggregate pain -----------------------------------------------------
equivalent_pain_df |> plot_equivalent_pain("Germany", 2024)
save_fig("equivalent_pain_Germany_2024.svg")

# Plot aggregate pain in the EU ---------
former_european_areas <- c(
  "Belgium-Luxembourg",
  "Czechoslovakia",
  "Yugoslav SFR"
)

EU_eggs_trade_matrix <- FAO_trade_matrix |>
  filter(item == "Hen eggs in shell, fresh") |>
  mutate(
    across(c(country, partner),
           \(x) if_else(x %in% c(EU_27, former_european_areas), "EU", x)),
    across(c(imports, exports),
           \(x) if_else(country == partner, 0, x))
  ) |>
  group_by(country, partner, year, item, unit) |>
  summarise(across(c(imports, exports),
            \(x) sum(x, na.rm = TRUE) ))

EU_egg_production_df <- production_df |>
  filter(animal == "hens", !(country %in% EU_27)) |>
  mutate(country = if_else(country == "European Union (27)", "EU", country)) |>
  select(country, year, production = n_eggs, production_kg) |>
  group_by(country, year) |>
  summarise(across(c(production, production_kg), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") |>
  mutate(item = "Hen eggs in shell, fresh")

EU_hen_df <- hen_df |>
  mutate(
    country = if_else(country %in% EU_27, "EU", country),
    across(starts_with("share_"), \(x) if_else(country == "EU", NA, x))
  ) |>
  group_by(country, year) |>
  summarise(across(matches("^(n_|share)"), sum), .groups = "drop") |>
  mutate(
    share_battery = coalesce(share_battery, n_battery / n_hens),
    share_enriched = coalesce(share_enriched, n_enriched / n_hens),
    share_barn = coalesce(share_barn, n_barn / n_hens),
    share_free_range = coalesce(share_free_range, n_free_range / n_hens),
    share_organic = coalesce(share_organic, n_organic / n_hens),
    share_cage = coalesce(share_cage, share_battery + share_enriched)
  )

egg_trade_and_production_df <- EU_eggs_trade_matrix |>
  left_join(EU_hen_df, by = c("country", "year")) |>
  left_join(EU_hen_df,
            by = join_by(partner == country, year),
            suffix = c("_country", "_partner")) |>
  left_join(EU_egg_production_df, by = c("country", "year", "item")) |>
  aggregate_egg_trade() |>
  mutate(
    consumption = production - exports + imports,
    imports_share = imports / consumption,
  ) |>
  make_egg_trade_df_longer() |>
  mutate(
    production = production_total * share,
    exports = exports_total * share,
    production_not_exported = production - exports,
    consumption = production_not_exported + imports,
  ) |>
  select(
    country, year, item, housing_type,
    share, production, imports, exports,
    production_not_exported, consumption,
    unit
  )

EU_equivalent_pain_df <- egg_trade_and_production_df |>
  prepare_egg_pain_df() |>
  aggregate_pain(pain_weights)

EU_equivalent_pain_df |> plot_equivalent_pain("EU", 2024)
save_fig("equivalent_pain_EU_2024.svg")

# Plot evolution of cage and import shares -------------------------------
egg_trade_df |> plot_cage_and_import_shares(country = "Sweden")

# Extract cage data from CIWF 2020 report ----------------------------------
ciwf_cage_df <- extract_ciwf_cage_data()

# Eurostat data on pig housing -----------------------------------------------
library(eurostat)

pig_data_path <- "data/raw/pig_housing.csv"
download_eurostat_pig_data(pig_data_path)
pig_data <- pig_data_path |> read_csv()

pig_df <- pig_data |>
  label_eurostat(fix_duplicated = T)
