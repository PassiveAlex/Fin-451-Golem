# fct_data_loader.R
# Loads RTL::dflong and reshapes it for use throughout the app.
#
# RTL::dflong structure:
#   date   : Date
#   series : character  — e.g., "CL01", "CL12", "NG03"
#   value  : numeric    — daily settlement price
#
# Contract numbering: {TICKER}{NN} where NN = 01 (front) … 36 (36th contract)

#' Load the raw RTL::dflong dataset
#' @return data.frame with columns: date, series, value
load_rtl_data <- function() {
  df <- RTL::dflong
  df$date <- as.Date(df$date)
  df
}

#' Extract the integer contract number from a series name
#' "CL01" -> 1, "NG12" -> 12
#' @param series character vector of RTL series names
#' @param prefix character — the ticker prefix (e.g., "CL")
#' @return integer vector
extract_contract_num <- function(series, prefix) {
  as.integer(sub(paste0("^", prefix), "", series))
}

#' Filter RTL long data to a single market and return tidy long format
#'
#' @param df data.frame from load_rtl_data()
#' @param prefix character — rtl_prefix from MARKETS list (e.g., "CL")
#' @param max_contracts integer — maximum contract number to retain (default 36)
#' @return data.frame: date, contract_num (int), price (dbl)
get_market_long <- function(df, prefix, max_contracts = 36L) {
  pattern <- paste0("^", prefix, "\\d{2}$")
  df |>
    filter(grepl(pattern, series)) |>
    mutate(contract_num = extract_contract_num(series, prefix)) |>
    filter(!is.na(contract_num), contract_num >= 1L, contract_num <= max_contracts) |>
    select(date, contract_num, price = value) |>
    arrange(date, contract_num)
}

#' Reshape long market data to wide format
#' Each column: {PREFIX}_C{N}, rows: one per date
#'
#' @param long_df data.frame from get_market_long()
#' @param prefix character — used to name columns
#' @return data.frame with date column + one column per contract
get_market_wide <- function(long_df, prefix) {
  long_df |>
    pivot_wider(
      names_from  = contract_num,
      names_prefix = paste0(prefix, "_C"),
      values_from = price
    ) |>
    arrange(date)
}

#' Compute daily log returns from a wide price dataframe
#'
#' Adds return columns named ret_{PREFIX}_C{N} for every price column found.
#' Returns are NA on the first observation.
#'
#' @param wide_df data.frame from get_market_wide()
#' @param prefix character
#' @return data.frame with date + return columns (ret_*)
compute_log_returns_wide <- function(wide_df, prefix) {
  price_cols <- grep(paste0("^", prefix, "_C\\d+$"), names(wide_df), value = TRUE)

  wide_df |>
    arrange(date) |>
    mutate(across(
      all_of(price_cols),
      ~ log(.x / lag(.x)),
      .names = "ret_{.col}"
    )) |>
    select(date, starts_with("ret_"))
}

#' Build the full price and returns data for one market
#'
#' @param prefix character — rtl_prefix from MARKETS
#' @param max_contracts integer
#' @return list with elements:
#'   $long  : tidy long-format prices (date, contract_num, price)
#'   $wide  : wide prices (date, {PREFIX}_C1 … {PREFIX}_C{N})
#'   $ret_wide : wide log returns (date, ret_{PREFIX}_C1 …)
build_market_data <- function(prefix, max_contracts = 36L) {
  raw   <- load_rtl_data()
  long  <- get_market_long(raw, prefix, max_contracts)
  wide  <- get_market_wide(long, prefix)
  ret   <- compute_log_returns_wide(wide, prefix)

  list(long = long, wide = wide, ret_wide = ret)
}

#' Filter a wide price dataframe to a date range
#'
#' @param wide_df data.frame with a date column
#' @param date_from Date
#' @param date_to   Date
#' @return filtered data.frame
filter_date_range <- function(wide_df, date_from, date_to) {
  wide_df |> filter(date >= date_from, date <= date_to)
}
