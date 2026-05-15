download_wrapper <- function(.download){
  force(.download)  # force evaluation to prevent lazy-eval quirks
  function(file_path) {
    if (!file.exists(file_path)){
      .download(file_path)
    } else {
      file_name <- file_path |> str_extract(r"([^/]+?$)")
      folder_path <- file_path |> str_extract(r"(/?(.+/)*)")
      print(glue("✓ `{file_name}` already downloaded in `{folder_path}`."))
    }
  }
}

download_production_data <- download_wrapper(function(file_path) {
  get_faostat_bulk("QCL") |> saveRDS(file_path)
})

download_trade_data <- download_wrapper(function(file_path) {
  TCL_url <- glue("https://bulks-faostat.fao.org/production/",
                  "Trade_CropsLivestock_E_All_Data_(Normalized).zip")
  # Using the URL because the normal API request was failing for some reason
  get_faostat_bulk_url(TCL_url) |> saveRDS(file_path)
})

download_trade_matrix <- download_wrapper(function(file_path) {
  # Select columns & filter rows for faster speed when reading file from disk
  get_faostat_bulk("TM")  |>
    select(
      reporter_countries,
      partner_countries,
      year,
      element,
      item,
      value,
      unit,
      flag
    ) |>
    filter(
      element %in% c("import_quantity", "export_quantity")
    ) |>
    saveRDS(file_path)
})

download_eurostat_pig_data <- download_wrapper(function(file_path) {
  get_eurostat("ef_ah_pigs") |> write_csv(file_path)
})

please_download_CIWF_report <- function(pdf_path){
  # PDF must be downloaded manually from the URL below and saved in `data/raw`.
  pdf_url <- "https://www.ciwf.org/media/7442226/ciwf_cage-age_report_oct-20_english_final.pdf"
  pdf_path <- "data/raw/ciwf_cage-age_report_oct-20_english_final.pdf"
  if (!file.exists(pdf_path)) {
    stop(glue("PDF not found at {pdf_path}. Download manually from: {pdf_url}"))
  }
}

convert_FAO_units <- function(FAO_df){
  converted_FAO_df <- FAO_df |>
    mutate(
      conversion_factor = case_match(unit,
                                     "1000 An" ~ 1000,
                                     "1000 No" ~ 1000,
                                     "t" ~ 1000,
                                     .default = 1),
      quantity = quantity * conversion_factor,
      unit = case_match(unit,
                        "1000 An" ~ "animals",
                        "1000 No" ~ "number",
                        "t" ~ "kg",
                        "An" ~ "animals",
                        "No" ~ "number",
                        .default = unit)
    )
  return(converted_FAO_df)
}

normalize_trading_country_names <- function(country_names) {
  special_cases <- c(
    "TÃ¼rkiye" = "Turkey",
    "RÃ©union" = "Réunion"
  )
  normalized_country_names <- countrycode(
    country_names,
    origin = "country.name.en",
    destination = "country.name.en",
    custom_match = special_cases,
    nomatch = NULL # keep the original country name if there's no match
  )
  return(normalized_country_names)
}

merge_regions_to_their_country <- function(trade_matrix) {
  country_level_df <- trade_matrix |>
    mutate(
      across(
        c(country, partner),
        ~ case_match(
          .x,
          "Réunion" ~ "France",
          "French Guiana" ~ "France",
          "Canton and Enderbury Islands" ~ "Kiribati",
          "Midway Island" ~ "United States",
          "Wake Island" ~ "United States",
          .default = .x
        )
      )
    ) |>
    filter(country != partner) |>
    group_by(country, partner, year, element, item, unit) |>
    summarise(quantity = sum(quantity), flag = unique(flag)) |>
    ungroup()

  return(country_level_df)
}

convert_liquid_eggs_to_n_eggs <- function(liquid_weight){
  #' Based on https://IncredibleEgg.org/Conversion:
  #' 50 whole eggs = 5 lbs 8 oz liquid eggs
  #'               = 5 * 16 + 8 oz
  #'               = 88 oz
  #'               = 88 * 0.028349523125 kg
  #'               = 2.494758 kg of liquid eggs
  #' So:
  #' 1 kg of liquid eggs = 50 / 2.494758
  #'                     = 20.04202 whole eggs
  n_eggs = liquid_weight * 20.04202
  return(n_eggs)
}

convert_dried_eggs_to_n_eggs <- function(dried_weight){
  #' Based on https://IncredibleEgg.org/Conversion:
  #' 200 whole eggs = 6 lbs 4 oz dried eggs
  #'                = 6 * 16 + 4 oz
  #'                = 100 oz
  #'                = 100 * 0.028349523125 kg
  #'                = 2.834952 kg of dried eggs
  #' So:
  #' 1 kg of dried eggs = 200 / 2.834952
  #'                    = 70.54793 whole eggs
  n_eggs = dried_weight * 70.54793
  return(n_eggs)
}

convert_shell_eggs_to_n_eggs <- function(shell_egg_weight){
  #' There are several standards for shell egg weight categories:
  #' https://en.wikipedia.org/wiki/Chicken_egg_sizes
  #' This conversion uses 63g per egg, which corresponds to the minimum weight
  #' of a Large egg in the EGG-1 UNECE standard.
  n_eggs = shell_egg_weight / 0.063
  return(n_eggs)
}

convert_egg_weight_to_n_eggs <- function(df){
  egg_units <- df |> filter(item |> str_detect("[Ee]ggs")) |> select(unit)
  stopifnot(all(egg_units == "kg"))

  converted_df <- df |>
    mutate(
      quantity = case_match(
        item,
        "Eggs, liquid" ~ convert_liquid_eggs_to_n_eggs(quantity),
        "Eggs, dried" ~ convert_dried_eggs_to_n_eggs(quantity),
        "Hen eggs in shell, fresh" ~ convert_shell_eggs_to_n_eggs(quantity),
        .default = quantity
      ),
      unit = if_else(item |> str_detect("[Ee]ggs"), "eggs", unit)
    )
  return(converted_df)
}

adjust_meat_preparations <- function(FAO_df) {
  #' Beef, pig and poultry meat _preparations_ include prepared meals that
  #' contain more than 20% of meat and offal by weight.
  #' ––> conservatively multiply these items by 0.2.

  #' Code to get list of items to multiply by 0.2:
  # read_dimension_metadata("TCL", "item") |>
  # filter(Description |> str_detect(glue("Includes prepared meals that",
  #                                       " contain more ?than 20% of meat",
  #                                       " and offal by weight")),
  #        Item |> str_detect("preparation")) |>
  # pull(Item)

  items_to_multiply_by_0.2 <- c("Beef and veal preparations nes",
                                "Pig meat preparations",
                                "Poultry meat preparations")

  adjusted_df <- FAO_df |> mutate(
    quantity = if_else(item %in% items_to_multiply_by_0.2,
                       quantity * 0.2,
                       quantity)
  )

  return(adjusted_df)
}

get_all_FAO_meat_items <- function(){
  #' The "TCL" dataset has an aggregate for all "Meat and Meat Preparation", but
  #' it only has monetary values, not quantities. To get total quantities of
  #' meat, get a list of all items included in the aggregate, and sum their
  #' quantities.
  all_meat_items <- read_fao(
    area_codes = "5000",  # World
    element_codes = "2920",  # Export value
    item_codes = "1885>",  # Meat and Meat Preparations > (List)
    year_codes = "2024",
    dataset = "TCL"
  ) |>
    select(Item) |>
    unique() |>
    pull()
  return(all_meat_items)
}

get_all_FAO_countries <- function(){
  all_countries <- read_fao(
    area_codes = "5000>",  # World > (List of countries in the "World" group)
    element_codes = "2620",  # Import value
    item_codes = "10",  # Total Merchandise Trade
    year_codes = "2024",
    dataset = "TCL"
  ) |>
    select(Area) |>
    unique() |>
    pull()
  return(all_countries)
}

prepare_production_df <- function(raw_production_df) {
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
  return(production_df)
}

prepare_trade_df <- function(raw_trade_df) {
  all_meat_items <- get_all_FAO_meat_items()

  trade_df <- raw_trade_df |>
    filter(
      item %in% all_meat_items,
      element %in% c("import_quantity", "export_quantity")
    ) |>
    select(
      area_code,
      country = area,
      year,
      item,
      element,
      quantity = value,
      unit
    ) |>
    convert_FAO_units() |>
    adjust_meat_preparations() |>
    pivot_wider(
      names_from = element,
      values_from = quantity
    ) |>
    rename(
      import = import_quantity,
      export = export_quantity
    ) |>
    mutate(net_export = export - import)
  return(trade_df)
}


prepare_trade_matrix <- function(raw_trade_matrix){
  trade_matrix <- raw_trade_matrix |>
    rename(
      country = reporter_countries,
      partner = partner_countries,
      quantity = value
    ) |>
    filter(
      item |> str_detect(glue(r"([pP]igs?\b)",
                              "[cC]hicken",
                              "Poultry meat preparations",
                              "Hen eggs",
                              "Eggs, (dried|liquid)",
                              .sep = "|")),
      !(item |> str_detect("^Feed"))
    ) |>
    convert_FAO_units() |>
    mutate(across(c(country, partner), normalize_trading_country_names)) |>
    merge_regions_to_their_country() |>
    mutate(
      animal = case_when(
        item |> str_detect("[Ee]gg") ~ "hens",
        item |> str_detect("[Cc]hicken") ~ "broilers",
        # Assume that poultry meat preparations are all made with chicken
        item == "Poultry meat preparations" ~ "broilers",
        item |> str_detect("[Pp]ig") ~ "pigs",
        .default = "other"
      )
    ) |>
    adjust_meat_preparations() |>
    convert_egg_weight_to_n_eggs()
  return(trade_matrix)
}

consolidate_trade_flow <- function(self_reported, third_party_reported){
  #' Consolidate an import or export into a single value
  #'
  #' This function takes a self-reported and third-party-reported value for an
  #' import or export, and returns a single value.
  #'
  #' The current approach is to:
  #' - select a non-missing value if possible,
  #' - select a non-null value if possible,
  #' - if two non-missing and non-null values are available, return the third-
  #'   party-reported one.
  #' The idea is that the overestimation of trade flows is less likely with a
  #' multitude of reporter countries than with a single reporter country.
  consolidated_value <- case_when(
    self_reported |> is.na() & third_party_reported |> is.na() ~ NA,
    self_reported |> is.na() ~ third_party_reported,
    third_party_reported |> is.na() ~ self_reported,
    self_reported == 0 ~ third_party_reported,
    third_party_reported == 0 ~ self_reported,
    .default = third_party_reported
  )
  return(consolidated_value)
}

aggregate_trade_matrix <- function(trade_matrix){
  third_party_aggregated_trade_matrix <- trade_matrix |>
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

  self_reported_aggregated_trade_matrix <- trade_matrix |>
    group_by(country, year, element, animal, unit) |>
    summarise(quantity = sum(quantity), .groups = "drop") |>
    pivot_wider(
      names_from = element,
      values_from = quantity
    ) |>
    rename(import = import_quantity, export = export_quantity)

  aggregated_trade_matrix <-
    full_join(
      self_reported_aggregated_trade_matrix,
      third_party_aggregated_trade_matrix,
      by = c("country", "year", "animal", "unit"),
      suffix = c("_self_reported", "_third_party")
    ) |>
    mutate(
      import = consolidate_trade_flow(import_self_reported, import_third_party),
      export = consolidate_trade_flow(export_self_reported, export_third_party),
      net_export = export - import
    )
  return(aggregated_trade_matrix)
}


extract_ciwf_cage_data <- function() {
  pdf_path <- "data/raw/ciwf_cage-age_report_oct-20_english_final.pdf"
  please_download_CIWF_report(pdf_path)

  pdf_content <- pdf_text(pdf_path)
  page_15_content <- pdf_content[15]
  ciwf_table_rows <- str_split(page_15_content, "\n")[[1]]

  data_row_pattern <- r"(.+?\d+.*)"
  row_has_data <- function(x) str_detect(x, data_row_pattern)

  thousands_separator <- ","
  percentage <- r"{\(\d.*?%\)}"
  inputation_flag <- r"{\*}"
  delete_pattern <- glue("{thousands_separator}|{percentage}|{inputation_flag}")

  column_names <- c(
    "country",
    "n_hens",
    "n_hens_caged",
    "n_rabbits",
    "n_rabbits_caged",
    "n_sows",
    "n_sows_gestation_crate",
    "n_sows_farrowing_crate",
    "n_caged_total"
  )

  ciwf_cage_df <- ciwf_table_rows |>
    keep(row_has_data) |>
    str_remove_all(delete_pattern) |>
    str_replace_all(r"(\s{2,})", ",") |>
    paste(collapse = "\n") |>
    read_csv(col_names = column_names) |>
    filter(country != "EU 27") |>
    mutate(country = countrycode(country, "country.name", "country.name.en"))

  return(ciwf_cage_df)
}


plot_global_meat_imports <- function(trade_df){
  global_meat_df <- trade_df |>
    filter(country == "World") |>
    group_by(year) |>
    summarise(
      across(c(import, export), ~ sum(.x, na.rm = TRUE)),
      unit = unique(unit),
    ) |>
    mutate(across(c(import, export), ~ .x / 10^9), unit = "Mt")

  global_meat_imports_plot <- global_meat_df |>
    ggplot(aes(year, import)) +
    geom_line() +
    labs(
      x = "",
      y = "",
      title = "Global importations of meat products (Mt)",
    ) +
    theme_minimal()
  return(global_meat_imports_plot)
}

plot_housing_shares <- function(hen_housing_df, country_name){
  housing_shares_plot_df <- hen_housing_df |>
    filter(country == country_name) |>
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
    select(country, year, housing_system, system_share)

  housing_shares_plot <- housing_shares_plot_df |>
    ggplot(aes(x = year, y = system_share, fill = fct_rev(housing_system))) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete(
      palette = c("#81CF72", "#FFCC22", "#EE9922", "#EE6666", "#B05555")
    ) +
    geom_text(aes(label = scales::percent(system_share, accuracy = 1)),
              position = position_stack(vjust = 0.5),
              size = 3) +
    labs(
      title = "Hen housing systems in {country_name}" |> glue(),
      x = "Year",
      y = "Share of laying hens",
      fill = "Housing system"
    ) +
    theme_minimal()

  return(housing_shares_plot)
}

map_net_exports <- function(
    trade_df,
    year = NULL,
    item = "Meat of chickens, fresh or chilled"
) {
  if(year |> is.null()) {year = max(trade_df$year)}

  worldmap_df <- map_data("world") |>
    mutate(country = countryname(region, warn=FALSE))

  net_export_df <- trade_df |>
    filter(country %in% get_all_FAO_countries()) |>
    mutate(country = countryname(country)) |>
    group_by(country) |>
    filter(
      year == !!year,
      item == !!item
    ) |>
    ungroup() |>
    mutate(
      across(where(is.double), ~ . / 10^9),
      unit = "Mt"
    )

  net_export_map <- net_export_df |>
    inner_join(worldmap_df, by = "country") |>
    ggplot(aes(x = long, y = lat, group = group)) +
    coord_fixed(1.2) +
    geom_polygon(aes(fill = net_export)) +
    scale_fill_gradient2(
      high = "#D13C4B",
      mid = "#FBF8B0",
      low = "#4288B5",
      midpoint = 0
    ) +
    theme_void() +
    labs(fill = glue("Net exports,\nchicken meat,\n{year} (Mt)"))
  return(net_export_map)
}

