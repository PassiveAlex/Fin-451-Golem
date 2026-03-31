# fct_volatility.R
# Volatility analytics: realized vol, EWMA, vol term structure, vol cone.
# Inputs are wide-format price or return data frames.

#' Compute log returns for selected contracts
#'
#' @param wide_df     data.frame — wide prices with `date` column
#' @param prefix      character
#' @param contract_ns integer vector — contract numbers to compute returns for
#' @return data.frame: date + one return column per contract (named ret_{PREFIX}_C{N})
compute_log_returns <- function(wide_df, prefix, contract_ns = c(1L, 3L, 6L, 12L)) {
  cols    <- paste0(prefix, "_C", contract_ns)
  present <- intersect(cols, names(wide_df))
  if (length(present) == 0L) return(data.frame(date = wide_df$date))

  wide_df |>
    dplyr::select(date, dplyr::all_of(present)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(dplyr::across(
      -date,
      ~ log(.x / dplyr::lag(.x)),
      .names = "ret_{.col}"
    )) |>
    dplyr::select(date, dplyr::starts_with("ret_"))
}

#' Rolling annualised realised volatility (using slider)
#'
#' @param x       numeric vector — log returns (may contain NAs)
#' @param window  integer — rolling window in trading days
#' @param ann     logical — annualise by sqrt(252)? (default TRUE)
#' @return numeric vector of same length as x; NA where window incomplete
rolling_realized_vol <- function(x, window = 63L, ann = TRUE) {
  vol <- slider::slide_dbl(
    x,
    ~ stats::sd(.x, na.rm = FALSE),
    .before    = window - 1L,
    .complete  = TRUE
  )
  if (ann) vol <- vol * sqrt(TRADING_DAYS)
  vol
}

#' EWMA volatility (RiskMetrics methodology)
#'
#' sigma^2_t = lambda * sigma^2_{t-1} + (1 - lambda) * r^2_{t-1}
#'
#' @param x      numeric vector — log returns
#' @param lambda numeric — decay factor (0.94 for daily per RiskMetrics)
#' @param ann    logical — annualise result?
#' @return numeric vector of annualised EWMA vol
ewma_vol <- function(x, lambda = 0.94, ann = TRUE) {
  n    <- length(x)
  var2 <- rep(NA_real_, n)

  first <- which(!is.na(x))[1L]
  if (is.na(first)) return(var2)

  var2[first] <- x[first]^2

  for (i in seq(first + 1L, n)) {
    if (!is.na(x[i]) && !is.na(var2[i - 1L])) {
      var2[i] <- lambda * var2[i - 1L] + (1 - lambda) * x[i]^2
    } else {
      var2[i] <- var2[i - 1L]
    }
  }

  out <- sqrt(var2)
  if (ann) out * sqrt(TRADING_DAYS) else out
}

#' Volatility term structure snapshot at a single date
#'
#' Returns realised vol for each available contract on the given date.
#'
#' @param wide_df     data.frame — wide prices
#' @param prefix      character
#' @param date_val    Date — snapshot date
#' @param window      integer — lookback window for vol calculation
#' @param max_c       integer — maximum contract number to include
#' @return data.frame: contract (int), vol (dbl annualised). NULL if date missing.
vol_term_structure_snapshot <- function(wide_df, prefix, date_val,
                                         window = 63L, max_c = 36L) {
  end_idx <- which(wide_df$date == date_val)
  if (length(end_idx) == 0L) return(NULL)

  start_idx <- max(1L, end_idx - window - 1L)
  cols      <- paste0(prefix, "_C", seq_len(max_c))
  present   <- intersect(cols, names(wide_df))
  if (length(present) == 0L) return(NULL)

  sub <- wide_df[start_idx:end_idx, present, drop = FALSE]

  vols <- vapply(present, function(col) {
    rets <- log(sub[[col]] / dplyr::lag(sub[[col]]))
    stats::sd(rets, na.rm = TRUE) * sqrt(TRADING_DAYS)
  }, numeric(1L))

  data.frame(
    contract = as.integer(sub(paste0(prefix, "_C"), "", present)),
    vol      = vols
  ) |> dplyr::filter(!is.na(vol))
}

#' Rolling volatility for multiple contracts over time
#'
#' @param wide_df     data.frame — wide prices
#' @param prefix      character
#' @param contract_ns integer vector — contracts to compute
#' @param window      integer
#' @return data.frame: date + vol_{PREFIX}_C{N} columns
rolling_vol_wide <- function(wide_df, prefix,
                              contract_ns = c(1L, 3L, 6L, 12L),
                              window = 63L) {
  ret_df  <- compute_log_returns(wide_df, prefix, contract_ns)
  ret_cols <- grep("^ret_", names(ret_df), value = TRUE)

  result <- ret_df[, "date", drop = FALSE]
  for (col in ret_cols) {
    vol_col <- sub("^ret_", "vol_", col)
    result[[vol_col]] <- rolling_realized_vol(ret_df[[col]], window)
  }
  result
}

#' Detect high-volatility regime periods
#'
#' A day is in "high vol" regime if its rolling vol exceeds the
#' trailing 1-year median vol.
#'
#' @param vol_vec numeric vector — rolling realised vol time series
#' @param median_window integer — window for rolling median (default 252)
#' @return logical vector
vol_regime_high <- function(vol_vec, median_window = 252L) {
  rolling_median <- slider::slide_dbl(
    vol_vec,
    stats::median,
    na.rm   = TRUE,
    .before = median_window - 1L,
    .complete = TRUE
  )
  vol_vec > rolling_median & !is.na(vol_vec) & !is.na(rolling_median)
}

#' Volatility cone: historical distribution at multiple horizons
#'
#' @param wide_df     data.frame — wide prices
#' @param prefix      character
#' @param contract_n  integer — which contract to use (default 1 = front)
#' @param horizons    integer vector — lookback windows in days
#' @param pctiles     numeric vector — quantiles to compute (0–1)
#' @return data.frame: horizon (int) + quantile columns (p10, p25, …)
build_vol_cone <- function(wide_df, prefix, contract_n = 1L,
                            horizons = c(10L, 21L, 63L, 126L, 252L),
                            pctiles  = c(0.10, 0.25, 0.50, 0.75, 0.90)) {

  col <- paste0(prefix, "_C", contract_n)
  if (!col %in% names(wide_df)) return(NULL)

  rets <- log(wide_df[[col]] / dplyr::lag(wide_df[[col]]))

  purrr::map_dfr(horizons, function(h) {
    vols <- slider::slide_dbl(
      rets,
      ~ stats::sd(.x, na.rm = FALSE) * sqrt(TRADING_DAYS),
      .before   = h - 1L,
      .complete = TRUE
    )
    vols <- vols[!is.na(vols)]
    q    <- stats::quantile(vols, pctiles, na.rm = TRUE)
    df   <- as.data.frame(t(q))
    names(df) <- paste0("p", as.integer(pctiles * 100))
    df$horizon <- h
    df
  })
}
