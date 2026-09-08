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

download_FAO_trade_data <- download_wrapper(function(file_path) {
  TCL_url <- glue("https://bulks-faostat.fao.org/production/",
                  "Trade_CropsLivestock_E_All_Data_(Normalized).zip")
  # Using the URL because the normal API request was failing for some reason
  get_faostat_bulk_url(TCL_url) |> saveRDS(file_path)
})

download_FAO_trade_matrix <- download_wrapper(function(file_path) {
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

download_BACI_data <- function() {
  # set max download time to 30 minutes to avoid timeout
  # (and restore default on exit)
  default_timeout <- getOption("timeout")
  options(timeout = 30 * 60)
  on.exit(options(timeout = default_timeout), add = TRUE)

  BACI_url <-
    "https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS92_V202601.zip"
  archive_extract(BACI_url, "data/raw/BACI")
}

read_BACI_csv_files <- function(con) {
  BACI_file_paths <- glue("data/raw/BACI/BACI_HS92_Y{1995:2024}_V202601.csv")
  duckdb_read_csv(con, "BACI_raw_files", files = BACI_file_paths)

  duckdb_read_csv(
    con,
    "BACI_country_codes",
    files = "data/raw/BACI/country_codes_V202601.csv"
  )

  # Read product codes csv with readr to circumvent a duckdb import type error
  read_csv("data/raw/BACI/product_codes_HS92_V202601.csv") |>
    filter(str_detect(code, "^[0-9]{6}$")) |>
    mutate(k = as.numeric(code), product_name = description, .keep = "none") |>
    copy_to(con, df = _, name = "BACI_product_codes", overwrite = TRUE)

  return()
}

prepare_BACI_data <- function(output_path) {
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

  dbExecute(con, "SET temp_directory='/tmp'")  # to prevent a macOS bug
  read_BACI_csv_files(con)
  country_codes <- tbl(con, "BACI_country_codes")
  product_codes <- tbl(con, "BACI_product_codes")

  exporter_codes <- country_codes |>
    select(i = country_code, exporter_iso3 = country_iso3)
  importer_codes <- country_codes |>
    select(j = country_code, importer_iso3 = country_iso3)

  BACI_df <- tbl(con, "BACI_raw_files") |>
    filter(!is.na(i), !is.na(j)) |>
    left_join(exporter_codes, by = "i") |>
    left_join(importer_codes, by = "j") |>
    left_join(product_codes, by = "k") |>
    select(-i, -j) |>
    rename(
      year = t,
      product_code = k,
      value = v,
      quantity = q
    ) |>
    filter(
      product_code |> between(010000, 019999) |  # live animals
      product_code |> between(020000, 029999) |  # meat
      product_code |> between(040000, 040899)    # dairy and eggs
    ) |>
    mutate(
      value = value * 1000,       # convert thousand USD to USD
      quantity = quantity * 1000  # convert tons to kilograms
    )

  collect(BACI_df) |> saveRDS(output_path)
}

delete_BACI_dumps <- function(BACI_dir = "data/raw/BACI") {
  if (dir.exists(BACI_dir)) {
    unlink(BACI_dir, recursive = TRUE)
  }
}

download_and_prepare_BACI_data <- download_wrapper(function(file_path) {
  download_BACI_data()
  prepare_BACI_data(file_path)
  delete_BACI_dumps()
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

normalize_FAO_country_names <- function(country_names) {
  special_cases <- c(
    "TÃ¼rkiye" = "Turkey",
    "RÃ©union" = "Réunion",
    # For China, follow OWID: https://docs.owid.io/projects/etl/data/faostat/#harmonization-of-country-name
    "China" = "China (FAO)",
    "China, mainland" = "China",
    # Prevent `countrycode` function matching "Australia and New Zealand" as
    # "Australia" or "New Zealand" (which would cause unwanted `NA`s)
    "Australia and New Zealand" = "Australia and New Zealand",
    # Keep former countries as is
    "Ethiopia PDR" = "Ethiopia PDR",
    "Sudan (former)" = "Sudan (former)",
    "USSR" = "USSR"
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

merge_regions_to_their_country <- function(FAO_trade_matrix) {
  country_level_df <- FAO_trade_matrix |>
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
  all_meat_items_txt <- "data/raw/all_FOA_meat_items.txt"
  if (file.exists(all_meat_items_txt)) {
    all_meat_items <- all_meat_items_txt |> readLines()
  }
  else {
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
    all_meat_items |> write(all_meat_items_txt)
  }
  return(all_meat_items)
}

get_all_FAO_countries <- function(){
  all_countries_txt <- "data/raw/all_FOA_countries.txt"
  if (file.exists(all_countries_txt)) {
    all_countries <- all_countries_txt |> readLines()
  }
  else {
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
    all_countries |> write(all_countries_txt)
  }
  return(all_countries)
}

create_animal_column <- function(df) {
  df_with_animal_column <- df |> mutate(
    animal = case_when(
      item |> str_detect("[Ee]gg") ~ "hens",
      item |> str_detect("[Cc]hicken") ~ "broilers",
      item == "Poultry meat preparations" ~ "poultry",
      item |> str_detect("[Pp]ig") ~ "pigs",
      .default = "other"
    )
  )
  return(df_with_animal_column)
}

fix_suspicious_zeros <- function(production_df){
  #' Impute values for cells with suspicious null values
  #'
  #' A few countries report abnormal null values for some production variables.
  #' Ideally, these should be replaced and imputed with some kind of regression.
  #' For now, I just replace suspicious zeros for broilers in Luxembourg with
  #' NAs (2018 - 2024). These missing values will be replaced with the latest
  #' available value (2017) by the `interpolate_and_extend_missings` function.
  less_suspicious_production_df <- production_df |>
    mutate(across(
      c("production_kg", "n_slaughtered"),
      ~ if_else(
        animal == "broilers" & country == "Luxembourg"
        & n_animals > 10^4 & .x == 0,
        NA,
        .x
      )
    ))
  return(less_suspicious_production_df)
}

interpolate_and_extend_missings <- function(production_df){
  #' Interpolate and extend missing values
  #'
  #' * Missing years are added,
  #' * NAs before and after the first or last non-missing value are replaced
  #'   with the first or last available value,
  #' * NAs between two non-missing values are interpolated linearly.
  year_range <- min(production_df$year):max(production_df$year)

  interpolated_production_df <- production_df |>
    group_by(country, animal) |>
    complete(year = year_range) |>
    mutate(across(
      c("n_animals", "production_kg", "n_eggs", "n_slaughtered"),
      ~ na.fill(.x, "extend")
    )) |>
    ungroup()
  return(interpolated_production_df)
}

prepare_production_df <- function(raw_production_df, scope) {
  production_df <- raw_production_df |>
    rename(quantity = value, country = area) |>
    filter(
      item %in% scope$items,
      element %in% scope$elements
    ) |>
    convert_FAO_units() |>
    create_animal_column() |>
    mutate(
      variable_name = case_when(
        element == "producing_animals_slaughtered" ~ "n_slaughtered",
        element %in% c("stocks", "laying") ~ "n_animals",
        item |> str_detect("eggs") & unit == "number" ~ "n_eggs",
        element == "production" ~ "production_{unit}" |> glue()
      )
    ) |>
    select(country, year, animal, variable_name, quantity) |>
    pivot_wider(
      names_from = variable_name,
      values_from = quantity
    ) |>
    arrange(country, year) |>
    fix_suspicious_zeros() |>
    interpolate_and_extend_missings() |>
    mutate(
      country = normalize_FAO_country_names(country),
      slaughter_rate = n_slaughtered / n_animals,
      weight_per_animal = production_kg / n_slaughtered,
      lifespan = (1 / slaughter_rate) * seas::year.length(year),  # days
      ADG = weight_per_animal * 1000 / lifespan  # grams per day
    )
  return(production_df)
}

prepare_FAO_trade_df <- function(raw_FAO_trade_df) {
  all_meat_items <- get_all_FAO_meat_items()

  FAO_trade_df <- raw_FAO_trade_df |>
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
    create_animal_column() |>
    pivot_wider(
      names_from = element,
      values_from = quantity
    ) |>
    rename(
      import = import_quantity,
      export = export_quantity
    ) |>
    mutate(net_export = export - import)
  return(FAO_trade_df)
}

reconcile_by_minimum <- function(self_reported, third_party_reported){
  #' Consolidate an import or export into a single value
  #'
  #' This function takes a self-reported and third-party-reported value for an
  #' import or export, and returns a single value:
  #' - if both values are missing, return a missing,
  #' - if only one value is available, take it,
  #' - if two values are available, conservatively take the minimum,
  #'   following @burkholz2019.
  reconciled_value <- if_else(
    self_reported |> is.na() | third_party_reported |> is.na(),
    coalesce(self_reported, third_party_reported),
    pmin(self_reported, third_party_reported)
  )
  return(reconciled_value)
}

reconcile_mirror_discrepencies <- function(FAO_trade_matrix){
  #' Present all trade flows as imports and reconcile mirror statistics
  self_reported_imports <- FAO_trade_matrix |>
    filter(element == "import_quantity") |>
    rename(imports_self_reported = quantity) |>
    select(-element)

  imports_as_reported_by_partner <- FAO_trade_matrix |>
    filter(element == "export_quantity") |>
    rename(partner = country, country = partner) |>  # swap country and partner
    rename(imports_third_party_reported = quantity) |>
    select(-element)

  reconciled_imports <-
    full_join(
      self_reported_imports,
      imports_as_reported_by_partner,
      by = c("country", "partner", "year", "animal", "item", "unit"),
      suffix = c(".self_reported_imports", ".third_party_reported_imports"),
      relationship = "one-to-one"
    ) |>
    mutate(imports = consolidate_trade_flow(imports_self_reported,
                                            imports_third_party_reported))
  return(reconciled_imports)
}

make_trade_matrix_symmetric <- function(reconciled_imports){
  #' Add an exports column to the imports-only reconciled trade matrix
  reconciled_exports <- reconciled_imports |>
    select(
      country = partner,
      partner = country,
      year,
      item,
      exports = imports,
      unit
    )

  reconciled_FAO_trade_matrix <- reconciled_imports |>
    full_join(reconciled_exports, relationship = "one-to-one") |>
    mutate(across(c("imports", "exports"), ~ replace_na(.x, 0)))

  return(reconciled_FAO_trade_matrix)
}

prepare_FAO_trade_matrix <- function(raw_FAO_trade_matrix){
  FAO_trade_matrix <- raw_FAO_trade_matrix |>
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
    mutate(across(c(country, partner), normalize_FAO_country_names)) |>
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
    convert_egg_weight_to_n_eggs() |>
    reconcile_mirror_discrepencies() |>
    make_trade_matrix_symmetric() |>
    relocate(imports, exports, .before = unit)
  return(FAO_trade_matrix)
}

adjust_ADG_for_trade_flows <- function(production_df, FAO_trade_matrix){
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
      suffix = c(".country", ".partner"),
      relationship = "many-to-one"
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
      n_slaughtered = unique(n_slaughtered.country),
      n_imported = sum(imports_n_animals),
      n_exported = sum(exports_n_animals),
      n_consumed = n_slaughtered + n_imported - n_exported,
      adjusted_ADG = (
        (n_slaughtered - n_exported) * unique(ADG.country)
        + sum(imports_n_animals * ADG.partner)
      ) / n_consumed,
      ADG = unique(ADG.country)
    ) |>
    mutate(
      ADG_trade_difference = adjusted_ADG - ADG
    )
  return(adjusted_ADG_df)
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

save_fig <- function(file_name, width = 600, height = 300, scale = 3.5) {
  ggsave(
    glue("figures/{file_name}"),
    width = width,
    height = height,
    scale = scale,
    units = "px"
  )
}

plot_global_meat_imports <- function(FAO_trade_df){
  global_meat_df <- FAO_trade_df |>
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
    FAO_trade_df,
    year = NULL,
    item = "Meat of chickens, fresh or chilled"
) {
  if(year |> is.null()) {year = max(FAO_trade_df$year)}

  worldmap_df <- map_data("world") |>
    mutate(country = countryname(region, warn=FALSE))

  net_export_df <- FAO_trade_df |>
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

  palette <- c(
    "#008A80",
    "#66D4CC",
    "#FEE4E4",
    "#FEA19E",
    "#FD817E",
    "#BE4A47",
    "#7F312F"
  )
  breaks = c(-0.5, 0, 1, 2, 3, 4)

  net_export_map <- net_export_df |>
    inner_join(worldmap_df, by = "country") |>
    ggplot(aes(x = long, y = lat, group = group)) +
    coord_fixed(1.2) +
    geom_polygon(aes(fill = net_export)) +
    scale_fill_stepsn(
      colours = palette,
      breaks = breaks,
      values = scales::rescale(breaks),
      limits = c(NA, 5),
      show.limits = TRUE,
      labels = \(x) round(x, digits = 1)
    ) +
    theme_void() +
    labs(fill = glue("Net exports,\nchicken meat,\n{year} (Mt)"))
  return(net_export_map)
}

map_ADG <- function(production_df, year = NULL) {
  if(year |> is.null()) {year = max(production_df$year)}

  worldmap_df <- map_data("world") |>
    mutate(country = countryname(region, warn=FALSE))

  ADG_map <- production_df |>
    filter(
      year == !!year,
      animal == "broilers",
    ) |>
    # Iceland's turnover rate is 25, which implies a lifespan of 2 weeks
    # --> exclude outlier
    mutate(ADG = if_else(country == "Iceland", NA, ADG)) |>
    inner_join(worldmap_df, by = "country") |>
    ggplot(aes(x = long, y = lat, group = group)) +
    coord_fixed(1.2) +
    geom_polygon(aes(fill = ADG)) +
    scale_fill_binned(
      palette = "viridis",
      breaks = c(5, 10, 20, 30),
      show.limits = TRUE,
      labels = \(x) round(x)
    ) +
    theme_void() +
    labs(fill = glue("Average daily\nweight gain,\n{year} (g/day)"))

  return(ADG_map)
}
