# fct_seasonality.R
# Seasonal pattern analysis: monthly return distributions, seasonal index,
# STL decomposition, and year-over-year overlays.

#' Compute monthly aggregate log returns
#'
#' @param wide_df    data.frame — wide prices with `date` column
#' @param prefix     character
#' @param contract_n integer — contract to use (default 1 = front month)
#' @return data.frame: year (int), month_num (int), month (ordered factor), monthly_ret (dbl)
compute_monthly_returns <- function(wide_df, prefix, contract_n = 1L) {
  col <- paste0(prefix, "_C", contract_n)
  if (!col %in% names(wide_df)) return(NULL)

  wide_df %>%
    dplyr::select(date, price = dplyr::all_of(col)) %>%
    dplyr::filter(!is.na(price)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      year      = lubridate::year(date),
      month_num = lubridate::month(date),
      month     = lubridate::month(date, label = TRUE, abbr = TRUE),
      month     = forcats::fct_inorder(as.character(month)),
      ret       = log(price / dplyr::lag(price))
    ) %>%
    dplyr::filter(!is.na(ret)) %>%
    dplyr::group_by(year, month_num, month) %>%
    dplyr::summarise(monthly_ret = sum(ret, na.rm = TRUE), .groups = "drop")
}

#' Seasonal index using ratio-to-centred-moving-average method
#'
#' For each calendar month, compute the average ratio of price to
#' 12-month centred moving average.
#'
#' @param wide_df    data.frame — wide prices
#' @param prefix     character
#' @param contract_n integer
#' @return data.frame: month_num (int), month (chr), seasonal_index (dbl),
#'   lower (25th pct), upper (75th pct)
seasonal_index <- function(wide_df, prefix, contract_n = 1L) {
  col <- paste0(prefix, "_C", contract_n)
  if (!col %in% names(wide_df)) return(NULL)

  wide_df %>%
    dplyr::select(date, price = dplyr::all_of(col)) %>%
    dplyr::filter(!is.na(price)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      month_num = lubridate::month(date),
      # 252-day centred rolling mean (≈ 1 trading year)
      ma = zoo::rollapply(price, width = 252L, FUN = mean,
                          fill = NA, align = "center", na.rm = TRUE)
    ) %>%
    dplyr::filter(!is.na(ma)) %>%
    dplyr::mutate(ratio = price / ma) %>%
    dplyr::group_by(month_num) %>%
    dplyr::summarise(
      month          = lubridate::month(month_num[1], label = TRUE, abbr = TRUE) %>% as.character(),
      seasonal_index = mean(ratio, na.rm = TRUE),
      lower          = stats::quantile(ratio, 0.25, na.rm = TRUE),
      upper          = stats::quantile(ratio, 0.75, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    dplyr::arrange(month_num)
}

#' STL decomposition on monthly aggregated prices
#'
#' Aggregates to monthly before STL to ensure clean annual seasonality.
#'
#' @param wide_df    data.frame — wide prices
#' @param prefix     character
#' @param contract_n integer
#' @param s_window   character or integer passed to stl() (default "periodic")
#' @return data.frame: date (approx month-end), trend, seasonal, remainder.
#'   Returns NULL on failure.
stl_decompose <- function(wide_df, prefix, contract_n = 1L, s_window = "periodic") {
  col <- paste0(prefix, "_C", contract_n)
  if (!col %in% names(wide_df)) return(NULL)

  monthly <- wide_df %>%
    dplyr::select(date, price = dplyr::all_of(col)) %>%
    dplyr::filter(!is.na(price)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      yr  = lubridate::year(date),
      mo  = lubridate::month(date)
    ) %>%
    dplyr::group_by(yr, mo) %>%
    dplyr::summarise(
      month_end = max(date),
      price     = mean(price, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    dplyr::arrange(yr, mo)

  if (nrow(monthly) < 24L) return(NULL)  # need >= 2 full years

  # Determine start year and month for ts()
  start_yr <- monthly$yr[1L]
  start_mo <- monthly$mo[1L]

  ts_obj <- stats::ts(monthly$price, start = c(start_yr, start_mo), frequency = 12L)

  tryCatch({
    stl_fit  <- stats::stl(ts_obj, s.window = s_window)
    comps    <- stl_fit$time.series
    data.frame(
      date      = monthly$month_end,
      trend     = as.numeric(comps[, "trend"]),
      seasonal  = as.numeric(comps[, "seasonal"]),
      remainder = as.numeric(comps[, "remainder"])
    )
  }, error = function(e) {
    warning("stl_decompose failed: ", conditionMessage(e))
    NULL
  })
}

#' Year-over-year normalised price overlay
#'
#' Each year's price series is normalised to its first non-NA value = 100.
#'
#' @param wide_df    data.frame — wide prices
#' @param prefix     character
#' @param contract_n integer
#' @return data.frame: year (int), day_of_year (int), date (Date), price (dbl), normalized (dbl)
yoy_normalized <- function(wide_df, prefix, contract_n = 1L) {
  col <- paste0(prefix, "_C", contract_n)
  if (!col %in% names(wide_df)) return(NULL)

  wide_df %>%
    dplyr::select(date, price = dplyr::all_of(col)) %>%
    dplyr::filter(!is.na(price)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      year        = lubridate::year(date),
      day_of_year = lubridate::yday(date)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(
      base       = dplyr::first(price[!is.na(price)]),
      normalized = price / base * 100
    ) %>%
    dplyr::ungroup()
}

#' Seasonal realised volatility: monthly distribution of rolling vol
#'
#' @param wide_df    data.frame — wide prices
#' @param prefix     character
#' @param contract_n integer
#' @param window     integer — rolling vol window
#' @return data.frame: month_num, month, vol — monthly distribution of vol values
seasonal_vol <- function(wide_df, prefix, contract_n = 1L, window = 21L) {
  col <- paste0(prefix, "_C", contract_n)
  if (!col %in% names(wide_df)) return(NULL)

  rets <- log(wide_df[[col]] / dplyr::lag(wide_df[[col]]))
  vol  <- rolling_realized_vol(rets, window = window, ann = TRUE)

  data.frame(
    date      = wide_df$date,
    vol       = vol,
    month_num = lubridate::month(wide_df$date),
    month     = lubridate::month(wide_df$date, label = TRUE, abbr = TRUE)
  ) %>%
    dplyr::filter(!is.na(vol))
}
