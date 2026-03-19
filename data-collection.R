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
hen_housing_df |>
  filter(country == "Luxembourg") |>
  mutate(across(matches("battery|enriched|barn|free_range|organic"),
                ~na_if(., 0))) |>
  select(where(~!all(is.na(.)))) |>
  pivot_longer(
    matches("share_(battery|enriched|barn|free_range|organic)"),
    names_to = "housing_system",
    names_prefix = "share_",
    names_transform = as_factor,
    values_to = "system_share"
  ) |>
  select(country, year, housing_system, system_share) |>
  ggplot(aes(x = year, y = system_share, fill = fct_rev(housing_system))) +
  labs(fill = "Housing system") +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(
    palette = c("#81CF72", "#FFCC22", "#EE9922", "#EE6666", "#B05555")
  ) +
  geom_text(aes(label = scales::percent(system_share, accuracy = 1)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(
    title = "Hen housing systems in Luxembourg",
    x = "Year",
    y = "Share of laying hens",
    fill = "Housing system"
  ) +
  theme_minimal()
