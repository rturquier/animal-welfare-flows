download_wrapper <- function(.download){
  force(.download)  # force evaluation to prevent lazy-eval quirks
  function(file_path) {
    if (!file.exists(file_path)){
      .download(file_path)
    } else {
      file_name <- file_path |> str_extract(r"([^/]+\.[a-zA-Z]{1,5}$)")
      folder_path <- file_path |> str_extract(r"(/?(.+/)*)")
      print(glue("✓ `{file_name}` already downloaded in `{folder_path}`."))
    }
  }
}

download_production_data <- download_wrapper(function(file_path) {
  get_faostat_bulk("QCL") |> saveRDS(file_path)
})

download_trade_data <- download_wrapper(function(file_path) {
  get_faostat_bulk("TM") |> saveRDS(file_path)
})

download_eurostat_pig_data <- download_wrapper(function(file_path) {
  get_eurostat("ef_ah_pigs") |> write_csv(file_path)
})

please_download_CIWF_report <- download_wrapper(function(pdf_path){
  # PDF must be downloaded manually from the URL below and saved in `data/raw`.
  pdf_url <- "https://www.ciwf.org/media/7442226/ciwf_cage-age_report_oct-20_english_final.pdf"
  pdf_path <- "data/raw/ciwf_cage-age_report_oct-20_english_final.pdf"
  if (!file.exists(pdf_path)) {
    stop(glue("PDF not found at {pdf_path}. Download manually from: {pdf_url}"))
  }
})

convert_FAO_units <- function(FAO_df){
  converted_FAO_df <- FAO_df |>
    mutate(
      conversion_factor = case_match(unit,
                                     "1000 An" ~ 1000,
                                     "1000 No" ~ 1000,
                                     "t" ~ 1000,
                                     .default = 1),
      value = value * conversion_factor,
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
