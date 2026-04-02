# fct_forward_curve.R
# Pure functions for forward curve analysis.
# All functions operate on wide-format price data frames (output of get_market_wide()).

#' Compute the scalar slope of the forward curve on a given date
#'
#' Slope = C{front} price - C{back} price.
#' Positive = backwardation, negative = contango.
#'
#' @param wide_df  data.frame — wide prices with a `date` column
#' @param prefix   character  — e.g., "CL"
#' @param date_val Date
#' @param front    integer    — front contract number (default 1)
#' @param back     integer    — back contract number (default 12)
#' @return numeric scalar, or NA if data unavailable
compute_curve_slope <- function(wide_df, prefix, date_val, front = 1L, back = 12L) {
  row <- wide_df[wide_df$date == date_val, , drop = FALSE]
  if (nrow(row) == 0L) return(NA_real_)

  col_f <- paste0(prefix, "_C", front)
  col_b <- paste0(prefix, "_C", back)
  if (!col_f %in% names(row) || !col_b %in% names(row)) return(NA_real_)

  as.numeric(row[[col_f]][1L]) - as.numeric(row[[col_b]][1L])
}

#' Classify curve shape from slope
#'
#' @param slope numeric scalar or vector
#' @param thresholds named numeric — names: steep_back, mild_back, mild_cont, steep_cont
#' @return character vector of labels
classify_curve_shape <- function(slope,
                                  thresholds = CURVE_THRESHOLDS) {
  dplyr::case_when(
    is.na(slope)                          ~ "Unknown",
    slope >=  thresholds["steep_back"]    ~ "Steep Backwardation",
    slope >=  thresholds["mild_back"]     ~ "Mild Backwardation",
    slope >=  thresholds["mild_cont"]     ~ "Flat",
    slope >=  thresholds["steep_cont"]    ~ "Mild Contango",
    TRUE                                  ~ "Steep Contango"
  )
}

#' Assign a CSS class name for regime badge colouring
regime_css_class <- function(regime) {
  switch(regime,
    "Steep Backwardation" = "badge-steep-back",
    "Mild Backwardation"  = "badge-mild-back",
    "Flat"                = "badge-flat",
    "Mild Contango"       = "badge-mild-cont",
    "Steep Contango"      = "badge-steep-cont",
    "badge-flat"
  )
}

#' Build forward curve snapshot for one date
#'
#' @param wide_df   data.frame — wide prices
#' @param prefix    character
#' @param date_val  Date
#' @param max_c     integer — maximum contract to include
#' @return data.frame: contract (int), price (dbl). Returns NULL if date not found.
build_curve_snapshot <- function(wide_df, prefix, date_val, max_c = 36L) {
  row <- wide_df[wide_df$date == date_val, , drop = FALSE]
  if (nrow(row) == 0L) return(NULL)

  cols    <- paste0(prefix, "_C", seq_len(max_c))
  present <- intersect(cols, names(wide_df))
  if (length(present) == 0L) return(NULL)

  data.frame(
    contract = as.integer(sub(paste0(prefix, "_C"), "", present)),
    price    = as.numeric(row[1L, present])
  ) %>% dplyr::filter(!is.na(price))
}

#' Compute daily curve slope and regime history
#'
#' @param wide_df data.frame — wide prices
#' @param prefix  character
#' @param front   integer
#' @param back    integer
#' @return data.frame: date, slope, regime. Returns NULL if columns missing.
curve_slope_history <- function(wide_df, prefix, front = 1L, back = 12L) {
  col_f <- paste0(prefix, "_C", front)
  col_b <- paste0(prefix, "_C", back)
  if (!all(c(col_f, col_b) %in% names(wide_df))) return(NULL)

  wide_df %>%
    dplyr::select(date, f = dplyr::all_of(col_f), b = dplyr::all_of(col_b)) %>%
    dplyr::filter(!is.na(f), !is.na(b)) %>%
    dplyr::mutate(
      slope  = f - b,
      regime = classify_curve_shape(slope)
    ) %>%
    dplyr::select(date, slope, regime)
}

#' Compute regime fraction per calendar year (for stacked bar chart)
#'
#' @param slope_history data.frame from curve_slope_history()
#' @return data.frame: year (int), regime (chr), fraction (dbl)
regime_fractions_by_year <- function(slope_history) {
  slope_history %>%
    dplyr::mutate(year = lubridate::year(date)) %>%
    dplyr::count(year, regime) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(fraction = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, regime, fraction)
}

#' Compute annualised roll yield (C1 → C2) as a fraction of spot price
#'
#' Roll yield = (C1 - C2) / C1 * 12  (annualised monthly roll)
#' Positive roll yield = backwardation (roll adds value).
#'
#' @param wide_df data.frame — wide prices
#' @param prefix  character
#' @return data.frame: date, c1, c2, roll_yield. Returns NULL if columns missing.
compute_roll_yield <- function(wide_df, prefix) {
  c1 <- paste0(prefix, "_C1")
  c2 <- paste0(prefix, "_C2")
  if (!all(c(c1, c2) %in% names(wide_df))) return(NULL)

  wide_df %>%
    dplyr::select(date,
                  c1 = dplyr::all_of(c1),
                  c2 = dplyr::all_of(c2)) %>%
    dplyr::filter(!is.na(c1), !is.na(c2), c1 > 0) %>%
    dplyr::mutate(roll_yield = (c1 - c2) / c1 * 12)
}

#' Carry proxy: annualised spread between C1 and C3 as % of C1
#'
#' @param wide_df data.frame
#' @param prefix  character
#' @param date_val Date
#' @return numeric scalar — annualised carry in decimal form (e.g., 0.03 = 3%)
compute_carry_proxy <- function(wide_df, prefix, date_val) {
  row  <- wide_df[wide_df$date == date_val, , drop = FALSE]
  if (nrow(row) == 0L) return(NA_real_)
  c1 <- paste0(prefix, "_C1"); c3 <- paste0(prefix, "_C3")
  if (!all(c(c1, c3) %in% names(row))) return(NA_real_)
  p1 <- as.numeric(row[[c1]][1L]); p3 <- as.numeric(row[[c3]][1L])
  if (is.na(p1) || is.na(p3) || p1 <= 0) return(NA_real_)
  # 2-month spread annualised over 6 (12/2)
  (p1 - p3) / p1 * 6
}
