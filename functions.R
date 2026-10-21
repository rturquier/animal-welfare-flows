# Global variables --------------------------------------------------------
EU_27 <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
  "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
  "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden"
)


# Downloads ---------------------------------------------------------------
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

download_EU_hen_data <- download_wrapper(function(file_path){
  #' Downloaded on 2026-09-10 from:
  #' https://agriculture.ec.europa.eu/farming/animal-products/eggs_en
  download.file(
    "https://agriculture.ec.europa.eu/document/download/3611ba2e-ad9f-4b39-83db-00be67dc4022_en?filename=laying-hens-by-way-of-keeping-2011-2025_en.xlsx",
    file_path
  )
})

download_UK_hen_data <- download_wrapper(function(file_path){
  #' Downloaded on 2026-10-02 from:
  #' https://www.gov.uk/government/statistics/egg-statistics
  download.file(
    "https://assets.publishing.service.gov.uk/media/6a69cdd30825cc51a6c37cba/Egg_production_dataset_20260730.ods",
    file_path
  )
})

download_OWID_hen_data <- download_wrapper(function(file_path){
  #' Downloaded on 2026-10-02 from:
  #' https://ourworldindata.org/grapher/eggs-cage-free
  download.file(
    "https://ourworldindata.org/grapher/eggs-cage-free.csv?v=1&csvType=full&useColumnShortNames=true",
    file_path
  )
})

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

please_download_WFI_cage_share_map <- function(data_path){
  #' Data downloaded on 2026-09-29 from the URL below
  WFI_cage_map_data_url <- "https://welfarefootprint.org/laying-hens/"
  if (!file.exists(data_path)) {
    stop(
      glue(
        "Data not found at {data_path}. ",
        "Download manually from: {WFI_cage_map_data_url}. ",
        "At the bottom of the map, click on the download icon, then choose ",
        "''Data''. In the new window, go to the Full data tab, and then click ",
        "''Download''."
      )
    )
  }
}

please_download_CIWF_report <- function(pdf_path){
  # PDF must be downloaded manually from the URL below and saved in `data/raw`.
  pdf_url <- "https://www.ciwf.org/media/7442226/ciwf_cage-age_report_oct-20_english_final.pdf"
  pdf_path <- "data/raw/ciwf_cage-age_report_oct-20_english_final.pdf"
  if (!file.exists(pdf_path)) {
    stop(glue("PDF not found at {pdf_path}. Download manually from: {pdf_url}"))
  }
}

# Wrangling ---------------------------------------------------------------
# ------- Hen housing shares -------
complete_with_dashboard_data <- function(EU_hen_df){
  #' Add data copied from individual dashboard PDFs
  #'
  #' At time of coding, this function only adds one row (France, 2024).
  egg_dashboard_df <- read_csv("data/manual/EU_eggs_dashboards.csv") |>
    mutate(country = countrycode(country, "eurostat", "country.name.en")) |>
    mutate(across(starts_with("percentage"),
                  ~ . / 100,
                  .names = "{str_replace(.col, 'percentage', 'share')}"),
           .keep = "unused") |>
    mutate(across(
      matches("share_(enriched|barn|free_range|organic)"),
      ~ round(. * n_hens),
      .names = "{str_replace(.col, 'share', 'n')}"
    ))

  missing_rows <- egg_dashboard_df |>
    anti_join(EU_hen_df, by = c("country", "year"))

  NA_rows <- EU_hen_df |>
    filter(
      if_any(-c("country", "year", "n_battery", "share_battery"),
             ~is.na(.))
    ) |>
    semi_join(x=egg_dashboard_df, y=_, by=c("country", "year"))

  EU_hen_df_with_dashboard_data <- EU_hen_df |>
    anti_join(NA_rows, by=c("country", "year")) |>
    bind_rows(missing_rows, NA_rows)
  return(EU_hen_df_with_dashboard_data)
}

convert_percentage_string_to_numeric_share <- function(string){
  #' e.g. "80%" --> 0.8
  string |>
    str_remove_all("%") |>
    as.numeric() |>
    (`/`)(100)
}

normalize_WFI_country_names <- function(country_names) {
  WFI_typos <- c(
    "Kyrkystan" = "Kyrgyzstan",
    "Kyrgystan" = "Kyrgyzstan",
    "Morrocco" = "Morocco",
    "Philipines" = "Philippines",
    "Taijikistan" = "Tajikistan"
  )
  normalized_country_names <- countrycode(
    country_names,
    origin = "country.name.en",
    destination = "country.name.en",
    custom_match = WFI_typos
  )
  return(normalized_country_names)
}

prepare_WFI_hens_df <- function(data_file_name){
  data_path <- glue("data/raw/", data_file_name)
  please_download_WFI_cage_share_map(data_path)

  raw_WFI_hens_df <- read_tsv(data_path, locale = locale(encoding = "UTF-16LE"))
  WFI_hens_df <- raw_WFI_hens_df |>
    select(
      country = Country,
      year = Year,
      share_cage = Cage,
      share_barn = Barn,
      share_free_range = "Free range",
      share_organic = Organic,
      unknown_percent = Unknown,
      source_name = Source,
      source_url = "Available at",
      n_hens = "Total Layers",
      n_farms = "Commercial Egg farms",
    ) |>
    mutate(
      country = normalize_WFI_country_names(country),
      across(starts_with("share_"), convert_percentage_string_to_numeric_share),
      across(starts_with("share"), \(x) if_else(unknown_percent == "100%",
                                                NA,
                                                x)  )
    )
  return(WFI_hens_df)
}

prepare_OWID_hen_data <- function(data_path){
  OWID_hen_data <- read_csv("data/raw/OWID_hen_data.csv") |>
    rename(percentage_cage_free = share_of_hens_cage_free) |>
    mutate(
      country = countryname(entity),
      share_cage = 1 - (percentage_cage_free / 100)
    ) |>
    select(country, year, share_cage) |>
    filter(!(country %in% EU_27))
  return(OWID_hen_data)
}

fix_Belgium_and_Poland_2014 <- function(EU_hen_df){
  #' Replace Belgium and Poland 2014 with NAs
  #'
  #' For Belgium and Poland, 2014 is nearly identical to 2013, but with `barn`
  #' and `free_range` swapped. This is very probably a mistake.
  be_2014_row <- which(EU_hen_df$country == "Belgium"
                       & EU_hen_df$year == 2014)
  pl_2014_row <- which(EU_hen_df$country == "Poland"
                       & EU_hen_df$year == 2014)
  all_columns_except_country_and_year <- EU_hen_df |>
    colnames() |>
    tail(-2)
  EU_hen_df[be_2014_row, all_columns_except_country_and_year] <- NA
  EU_hen_df[pl_2014_row, all_columns_except_country_and_year] <- NA
  return(EU_hen_df)
}

prepare_EU_hen_df <- function(EU_hen_path){
  hen_housing_numbers <- read_excel(
    EU_hen_path,
    range = "A4:H408",
    col_names = c("country", "year", "n_battery", "n_enriched", "n_free_range",
                  "n_barn", "n_organic", "n_hens")
  )
  hen_housing_shares <- read_excel(
    EU_hen_path,
    range = "L4:P408",
    col_names = c("share_battery", "share_enriched", "share_free_range",
                  "share_barn", "share_organic")
  )

  EU_hen_df <- hen_housing_numbers |>
    bind_cols(hen_housing_shares) |>
    fill(country) |>
    mutate(country = countryname(country)) |>
    complete_with_dashboard_data() |>
    distinct(country, year, .keep_all = TRUE) |>
    arrange(country, year) |>
    relocate(n_free_range, .after = n_barn) |>
    relocate(share_free_range, .after = share_barn) |>
    mutate(
      across(matches("year|n_.+"), as.integer),
      share_battery = if_else(!is.na(share_enriched),
                              replace_na(share_battery, 0),
                              share_battery)
    ) |>
    fix_Belgium_and_Poland_2014()
  return(EU_hen_df)
}

share_of_remaining_batteries <- function(year, ban_year){
  #' Assume a 10-year linear transition away from conventional cages
  (ban_year - year) |>
    pmin(10) |>  # Ten years before the ban, all cages are conventional
    pmax(0) |>  # On the year of the ban, all cages are enriched
    (`/`)(10)
}

split_cages_between_conventional_and_enriched <- function(hen_df){
  battery_ban_df <- read_csv("data/manual/battery_bans.csv") |>
    select(country, year_of_battery_ban)

  hen_df_with_cage_split <- hen_df |>
    full_join(battery_ban_df, by = "country") |>
    fill(year_of_battery_ban, .direction = "downup") |>
    mutate(
      share_battery = case_when(
        !is.na(share_battery) | !is.na(share_enriched) ~ share_battery,
        # Assume linear transition over 10 years for the UK, Norway, Ukraine,
        # Turkey and Northe Macedonia, as these countries have at least some
        # enriched cages.
        !is.na(year_of_battery_ban) & !is.na(share_cage) ~ (
          share_of_remaining_batteries(year, year_of_battery_ban) * share_cage
        ),
        # For other non-EU partners, assume 100% of cages are conventional.
        is.na(year_of_battery_ban) & !is.na(share_cage) ~ share_cage,
        .default = NA
      ),
      share_enriched = if_else(
        is.na(share_enriched) & !is.na(share_battery) & !is.na(share_cage),
        share_cage - share_battery,
        share_enriched
      )
    )
  return(hen_df_with_cage_split)
}

combine_hen_housing_data_sources <- function(
    manual_cage_shares_df, EU_hen_df, WFI_hens_df, OWID_hen_df
){
  combined_hen_df <- bind_rows(   # The order matters here:
    manual_cage_shares_df,  # manually populated values are preferred
    EU_hen_df,              # over EU commission data,
    WFI_hens_df,            # WFI data provides values for non-EU countries,
    OWID_hen_df             # and OWID data is a fallback.
  ) |>
    group_by(country, year) |>
    slice_head(n = 1) |>  # Keep first of duplicate rows in order of appearance
    relocate(share_battery, share_enriched, .after = share_cage)

  return(combined_hen_df)
}

fill_missing_housing_shares_rowwise <- function(combined_hen_df){
  # Fill free range, barn and organic shares as 1 - the sum of other shares
  rowwise_filled_hen_df <- combined_hen_df |>
    rowwise() |>
    mutate(
      share_free_range = if_else(
        is.na(share_free_range) & !is.na(share_battery),
        1 - sum(c_across(share_battery:share_organic), na.rm = TRUE),
        share_free_range
      ),
      share_barn = if_else(
        is.na(share_barn) & !is.na(share_free_range),
        1 - sum(c_across(share_battery:share_organic), na.rm = TRUE),
        share_barn
      ),
      share_organic = if_else(
        is.na(share_organic) & !is.na(share_barn),
        1 - sum(c_across(share_battery:share_organic), na.rm = TRUE),
        share_organic
      )
    ) |>
    ungroup()
  return(rowwise_filled_hen_df)
}

impute_missing_hen_housing_data <- function(combined_hen_df){
  earliest_year_in_EU_data <-
    combined_hen_df |> filter(country %in% EU_27) |> pull(year) |> min()
  latest_year_in_EU_data <-
    combined_hen_df |> filter(country %in% EU_27) |> pull(year) |> max()
  year_range <- earliest_year_in_EU_data:latest_year_in_EU_data

  completed_df <- combined_hen_df |>
    fill_missing_housing_shares_rowwise() |>
    filter(!if_all(share_battery:share_organic, is.na)) |>
    mutate(hen_housing_shares_imputed = FALSE) |>
    group_by(country) |>
    complete(year = year_range) |>
    mutate(
      hen_housing_shares_imputed = replace_na(hen_housing_shares_imputed, TRUE),
      share_cage = coalesce(share_cage, share_battery + share_enriched),
      across(share_cage:share_organic, \(x) na.fill(x, "extend"))
    ) |>
    split_cages_between_conventional_and_enriched() |>
    mutate(
    ) |>
    filter(year %in% earliest_year_in_EU_data:latest_year_in_EU_data)

  return(completed_df)
}

prepare_hen_df <- function(){
  manual_cage_shares_df <- read_csv("data/manual/cage_shares.csv")

  EU_hen_path <- "data/raw/laying-hens-by-way-of-keeping-2011-2025_en.xlsx"
  download_EU_hen_data(EU_hen_path)
  EU_hen_df <- prepare_EU_hen_df(EU_hen_path)

  WFI_hens_df <- prepare_WFI_hens_df("Maps_Full Data_data.csv")

  OWID_hen_path <- "data/raw/OWID_hen_data.csv"
  download_OWID_hen_data(OWID_hen_path)
  OWID_hen_df <- prepare_OWID_hen_data(OWID_hen_path)

  hen_df <-
    combine_hen_housing_data_sources(manual_cage_shares_df, EU_hen_df,
                                     WFI_hens_df, OWID_hen_df) |>
    impute_missing_hen_housing_data()
  return(hen_df)
}

# ------- FAO data -------
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
    mutate(imports = reconcile_by_minimum(imports_self_reported,
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

aggregate_the_EU <- function(adjusted_ADG_df, production_df){
  EU_production_df <- production_df |>
    filter(
      country %in% EU_27,
      animal == "broilers"
    ) |>
    group_by(year) |>
    summarise(across(c(n_slaughtered, n_animals, production_kg), sum)) |>
    mutate(
      country = "EU_27",
      slaughter_rate = n_slaughtered / n_animals,
      weight_per_animal = production_kg / n_slaughtered,
      lifespan = (1 / slaughter_rate) * seas::year.length(year),  # days
      ADG = weight_per_animal * 1000 / lifespan  # grams per day
    ) |>
    rename_with(\(x) glue("{x}.country"), !c(country, year))

  former_european_areas <- c(
    "Belgium-Luxembourg",
    "Czechoslovakia",
    "Yugoslav SFR"
  )

  EU_df <- adjusted_ADG_df |>
    filter(country %in% EU_27) |>
    filter(!(partner %in% c(EU_27, former_european_areas))) |>
    group_by(partner, year) |>
    summarise(across(c(imports_n_animals, exports_n_animals), sum)) |>
    mutate(country = "EU_27") |>
    relocate(country) |>
    left_join(
      EU_production_df, by = c("country", "year"), relationship = "many-to-one"
    )

  adjusted_ADG_with_EU_df <- adjusted_ADG_df |>
    filter(!(country %in% EU_27), !(partner %in% EU_27)) |>
    add_row(EU_df) |>
    group_by(partner, year) |>
    fill(ends_with(".partner")) |>
    ungroup(year) |>
    mutate(across(ends_with(".partner"), \(x) na.fill(x, "extend"))) |>
    ungroup()

  return(adjusted_ADG_with_EU_df)
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
    aggregate_the_EU(production_df) |>
    group_by(country, year) |>
    summarise(
      n_slaughtered = unique(n_slaughtered.country),
      n_imported = sum(imports_n_animals),
      n_exported = sum(exports_n_animals),
      n_consumed = n_slaughtered + n_imported - n_exported,
      partner_ADG = sum(imports_n_animals * ADG.partner) / n_imported,
      adjusted_ADG = (
        (n_slaughtered - n_exported) * unique(ADG.country)
        + sum(imports_n_animals * ADG.partner)
      ) / n_consumed,
      ADG = unique(ADG.country),
      ADG_times_n_slaughtered = ADG * n_slaughtered,
      adjusted_ADG_times_n_consumed = adjusted_ADG * n_consumed,
    ) |>
    mutate(
      ADG_trade_difference = adjusted_ADG - ADG,
      share_of_imports_in_consumption = n_imported / n_consumed
    )
  return(adjusted_ADG_df)
}

# ------- Hen housing + trade data -------
make_egg_trade_df_longer <- function(egg_trade_df){
  housing_levels <- c("battery", "enriched", "barn", "free_range", "organic")
  any_housing_type <- "(battery|enriched|barn|free_range|organic)"

  longer_egg_trade_df <- egg_trade_df  |>
    rename_with(\(x) str_remove(x, "_country$")) |>
    pivot_longer(
      matches(glue("_{any_housing_type}$")),
      names_to = c("variable_name", "housing_type"),
      names_sep = glue("_(?={any_housing_type})"),
      values_to = "n_eggs"
    ) |>
    rename_with(\(x) glue("{x}_total"),
                c(production, consumption, imports, exports)) |>
    pivot_wider(names_from = "variable_name", values_from = "n_eggs") |>
    rename(n_hens_total = n_hens, n_hens = n) |>
    mutate(housing_type = factor(housing_type, levels = housing_levels))

  return(longer_egg_trade_df)
}

aggregate_egg_trade <- function(egg_trade_df){
  egg_trade_df |>
    group_by(country, year, item) |>
    summarise(
      imports_battery = sum(imports * share_battery_partner, na.rm = TRUE),
      imports_enriched = sum(imports * share_enriched_partner, na.rm = TRUE),
      imports_barn = sum(imports * share_barn_partner, na.rm = TRUE),
      imports_free_range = sum(imports * share_free_range_partner, na.rm=TRUE),
      imports_organic = sum(imports * share_organic_partner, na.rm = TRUE),
      imports = sum(imports),
      exports = sum(exports),
      unit = unique(unit),
      across(ends_with("_country"), unique),
      across(c(production, production_kg), unique),
      .groups = "drop"
  )
}

prepare_egg_trade_df <- function(production_df, FAO_trade_matrix, hen_df){
  egg_production_df <- production_df |>
    filter(animal == "hens") |>
    select(country, year, production = n_eggs, production_kg) |>
    mutate(item = "Hen eggs in shell, fresh")

  egg_trade_df <- FAO_trade_matrix |>
    filter(
      item |> str_detect("(E|e)ggs"),
    ) |>
    left_join(hen_df, by = c("country", "year")) |>
    left_join(hen_df,
              by = join_by(partner == country, year),
              suffix = c("_country", "_partner")) |>
    left_join(egg_production_df, by = c("country", "year", "item")) |>
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
    ) |>
    filter(country %in% EU_27, year >= 2011)
  return(egg_trade_df)
}

prepare_WFI_pain_per_hen_data <- function(){
  #' @gautron2021 :
  #' > "Many flocks already produce close to 500 eggs per hens in a lifetime"
  eggs_per_hen <- 500  # Assume 500 eggs per hen

  pain_levels <- c("annoying", "hurtful", "disabling", "excruciating")

  hours_of_pain_per_hen_df <-
    read_csv("data/manual/hours_of_pain_per_hen.csv") |>
    pivot_longer(
      c(central_estimate, lower_bound_of_95p_CI, higher_bound_of_95p_CI),
      names_to = "value_type",
      values_to = "pain_per_hen"
    ) |>
    mutate(
      pain_type = factor(pain_type, levels = pain_levels),
      housing_type = case_match(housing_type,
                                "conventional" ~ "battery",
                                "furnished" ~ "enriched",
                                "cage-free aviary" ~ "barn"),
      pain_per_hen = duration(pain_per_hen, units = "hours"),
      pain_per_egg = pain_per_hen / .env$eggs_per_hen
    )
  return(hours_of_pain_per_hen_df)
}

prepare_egg_pain_df <- function(egg_trade_df){
  hours_of_pain_per_hen_df <- prepare_WFI_pain_per_hen_data()

  egg_pain_df <- egg_trade_df |>
    semi_join(hours_of_pain_per_hen_df, by = "housing_type") |>
    left_join(
      hours_of_pain_per_hen_df,
      by = "housing_type",
      relationship = "many-to-many"
    ) |>
    mutate(
      pain_produced = pain_per_egg * production,
      pain_consumed = pain_per_egg * consumption,
      pain_imported = pain_per_egg * imports,
      pain_exported = pain_per_egg * exports,
    ) |>
    group_by(country, year, pain_type, value_type) |>
    summarise(across(matches("pain_......ed"), \(x) sum(x, na.rm = TRUE))) |>
    mutate(across(matches("pain_......ed"), as.duration))

  return(egg_pain_df)
}

aggregate_pain <- function(pain_df, pain_weights){
  pain_weights_df <- tibble(
    pain_type = egg_pain_df |> pull(pain_type) |> unique(),
    pain_weight = pain_weights
  )
  equivalent_pain_df <- egg_pain_df |>
    group_by(country, year, value_type) |>
    left_join(pain_weights_df, by = "pain_type") |>
    summarise(across(matches("pain_......ed"),
                     \(x) sum(x * pain_weight) )) |>
    mutate(across(matches("pain_......ed"), as.duration))
  return(equivalent_pain_df)
}


# ------- Other -------
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


# Plots -------------------------------------------------------------------
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

plot_housing_shares <- function(hen_df, country_name){
  housing_shares_plot_df <- hen_df |>
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

map_hen_housing <- function(hen_df, year = NULL){
  if(year |> is.null()) {year = max(production_df$year)}

  worldmap_df <- map_data("world") |>
    mutate(country = countryname(region, warn=FALSE))

  hen_housing_map <- hen_df |>
    filter(year == .env$year) |>
    inner_join(worldmap_df, by = "country") |>
    ggplot(aes(x = long, y = lat, group = group)) +
    coord_fixed(1.2) +
    geom_polygon(aes(fill = share_enriched, color = "#222")) +
    scale_fill_binned(
      palette = "Oranges",
      breaks = 1:8/10,
      show.limits = TRUE,
      # labels = \(x) glue(round(x * 100), " %")
    ) +
    guides(color="none") +
    theme_void() +
    labs(fill = glue("Share of hens in cages in {year}"))
  ggplot()
  return(hen_housing_map)
}

plot_apparent_consumption_by_system <- function(egg_trade_df, country, year){
  any_housing_type <- "(battery|enriched|barn|free_range|organic)"

  egg_trade_df |>
    filter(
      item == "Hen eggs in shell, fresh",
      country == .env$country,
      year == .env$year
    ) |>
    mutate(exports = -exports) |>  # To show exports below x-axis
    pivot_longer(
      c(imports, exports, production_not_exported),
      names_to = "variable_name",
      values_to = "n_eggs"
    ) |>
    mutate(variable_name = factor(variable_name,
                                  levels = c("imports",
                                             "production_not_exported",
                                             "exports")   )) |>
    ggplot(aes(fill=variable_name, y=n_eggs, x=housing_type)) +
    geom_bar(position="stack", stat="identity") +
    scale_fill_manual(
      name = NULL,
      values = c("#E92", "#0F5E89", "#1894D8")
    ) +
    labs(
      x = "Housing type",
      y = "Number of eggs",
      title = glue("Apparent consumption of eggs in {country}, {year}")
    ) +
    theme_minimal()
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
      year == .env$year,
      item == .env$item
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
      year == .env$year,
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

plot_ADG_by_country <- function(
    adjusted_ADG_df, year=NULL, only_top_n=177, compare_to=adjusted_ADG
){
  if(year |> is.null()) {year = max(production_df$year)}
  compare_to = enexpr(compare_to)

  adjusted_ADG_df |>
    filter(year == .env$year, country != "Iceland") |>
    ungroup() |>
    arrange(desc(!!compare_to)) |>
    slice_head(n = only_top_n) |>
    ggplot() +
    geom_segment(
      aes(x = ADG, xend = !!compare_to, y =  reorder(country, !!compare_to)),
      color = "#222", linewidth = 0.6
    ) +
    geom_point(aes(x = ADG, y = country, color = "ADG"), shape = 19) +
    geom_point(
      aes(x = !!compare_to, y = country, color = "adjusted_ADG"),
      shape = 17
    ) +
    scale_color_manual(
      name = NULL,
      values = c("ADG" = "#FFCC22", "adjusted_ADG" = "#EE9922")
    ) +
    labs(
      x = glue("ADG in {year} (g / day)"),
      y = NULL,
      title = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "top")
}

scatterplot_ADG_by_country <- function(adjusted_ADG_df, year=2024){
  adjusted_ADG_df |>
    filter(year == .env$year, country != "Iceland") |>
    ggplot() +
    geom_point(
      aes(
        x = ADG,
        y = partner_ADG,
        size = n_consumed,
        # color = share_of_imports_in_consumption > 0.1,
      ),
      alpha = 0.4,
    ) +
    geom_abline(alpha = 0.5) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      x="Average daily weight gain of chicken produced in the country (g/day)",
      y="ADG of consumed chicken (g/day)"
    ) +
    guides(color = "none", size = "none") +
    theme_minimal()
}

plot_ADG_evolution <- function(adjusted_ADG_df, country){
  country_name <- if_else(country == "EU_27", "the EU", country)

  ADG_evolution <- adjusted_ADG_df |>
    filter(country == .env$country) |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = ADG, color = "Production-based ADG")) +
    geom_point(aes(y = ADG, color = "Production-based ADG")) +
    geom_line(aes(y = adjusted_ADG, color = "Consumption-based ADG")) +
    geom_point(aes(y = adjusted_ADG, color = "Consumption-based ADG")) +
    scale_color_manual(
      name = NULL,
      values = c("Production-based ADG" = "#FFCC22",
                 "Consumption-based ADG" = "#EE9922")
    ) +
    labs(
      y = "Average Daily Gain (g / day)",
      title = glue("Production- and consumption-based ",
                   "broiler ADG in {country_name}")
    ) +
    theme_minimal()

  return(ADG_evolution)
}

plot_imported_pain_by_type <- function(egg_pain_df, country, year){
  pain_plot_df <- egg_pain_df |>
    filter(country == .env$country, year == .env$year) |>
    mutate(across(matches("pain_......ed"),
                  \(x) as.numeric(x, units = "years") )) |>
    pivot_wider(
      names_from = value_type,
      values_from = matches("pain_......ed")
    )

  pain_plot <- pain_plot_df |>
    ggplot(aes(y = pain_type)) +
    geom_pointrange(
      aes(
        x = pain_imported_central_estimate,
        xmin = pain_imported_lower_bound_of_95p_CI,
        xmax = pain_imported_higher_bound_of_95p_CI
      )
    ) +
    scale_x_log10(
      breaks = 10^(1:6),
      minor_breaks = c(outer(1:9, 10^(0:9))),  # 1, 2, 3, …, 9, 10, 20, 30 …
      labels = scales::label_log(),
      limits = c(1, NA),
    ) +
    labs(
      x = glue("years of pain by type imported in {country} in {year}"),
      y = "pain type"
    ) +
    theme_minimal()

  return(pain_plot)
}

plot_cage_and_import_shares <- function(egg_trade_df, country){
  cage_share_df <- egg_trade_df |>
    filter(
      year >= 2011,
      item == "Hen eggs in shell, fresh",
      country == .env$country
    ) |>
    mutate(
      housing_type = case_match(as.character(housing_type),
                                c("battery", "enriched") ~ "cage",
                                c("barn", "free_range", "organic") ~ "cage_free",
                                .default = as.character(housing_type))
    ) |>
    group_by(country, year, housing_type) |>
    summarise(across(share:consumption, sum)) |>
    pivot_wider(names_from = housing_type, values_from = share:consumption) |>
    mutate(
      imports_cage_share = imports_cage / (imports_cage + imports_cage_free),
      imports_share = (imports_cage + imports_cage_free) / (consumption_cage / consumption_cage_free)
    )

  cage_and_import_shares_plot <- cage_share_df |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = share_cage, color = "cages")) +
    geom_point(aes(y = share_cage, color = "cages")) +
    geom_line(aes(y = imports_share, color = "imports")) +
    geom_point(aes(y = imports_share, color = "imports")) +
    scale_color_manual(
      name = NULL,
      values = c("cages" = "#EE9922", "imports" = "#FFCC22")
    ) +
    labs(
      y = "share",
      title = glue("Share of cage egg production and share of imports",
                   " in {country}")
    ) +
    theme_minimal()
  return(cage_and_import_shares_plot)
}

