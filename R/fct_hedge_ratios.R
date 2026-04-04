# fct_hedge_ratios.R
# Hedge ratio analytics: rolling OLS, minimum-variance ratios, term structure,
# and basis risk decomposition.

#' Rolling OLS hedge ratio: exposure ~ hedge
#'
#' Regresses exposure_returns ~ hedge_returns over a rolling window.
#' The slope (beta) is the minimum-variance hedge ratio.
#'
#' @param x_ret  numeric vector — exposure (what you own / are hedging)
#' @param y_ret  numeric vector — hedge instrument returns (same length)
#' @param window integer — rolling window in days
#' @return data.frame: beta (dbl), r_squared (dbl), resid_sd (dbl), ci_lower (dbl), ci_upper (dbl)
ols_hedge_ratio <- function(x_ret, y_ret, window = 63L) {
  stopifnot(length(x_ret) == length(y_ret))
  n    <- length(x_ret)
  beta <- r2 <- resid <- ci_lo <- ci_hi <- rep(NA_real_, n)

  for (i in seq(window, n)) {
    xi <- x_ret[(i - window + 1L):i]
    yi <- y_ret[(i - window + 1L):i]
    ok <- !is.na(xi) & !is.na(yi)
    if (sum(ok) < max(10L, window %/% 3L)) next

    fit   <- stats::lm(yi[ok] ~ xi[ok])
    cf    <- stats::coef(fit)
    smry  <- summary(fit)

    beta[i]   <- cf[2L]
    r2[i]     <- smry$r.squared
    resid[i]  <- smry$sigma
    # 95% confidence interval for beta
    se_beta   <- smry$coefficients[2L, 2L]
    ci_lo[i]  <- beta[i] - 1.96 * se_beta
    ci_hi[i]  <- beta[i] + 1.96 * se_beta
  }

  data.frame(beta = beta, r_squared = r2,
             resid_sd = resid, ci_lower = ci_lo, ci_upper = ci_hi)
}

#' Analytical minimum-variance hedge ratio
#'
#' h* = rho * (sigma_S / sigma_F)
#' where S = spot/exposure (x), F = futures/hedge instrument (y).
#' Represents the number of futures contracts per unit of spot exposure
#' that minimises the variance of the hedged position.
#'
#' @param sigma_x numeric — volatility of exposure (S)
#' @param sigma_y numeric — volatility of hedge instrument (F)
#' @param rho     numeric — correlation between x and y
#' @return numeric scalar
min_variance_hedge_ratio <- function(sigma_x, sigma_y, rho) {
  rho * (sigma_x / sigma_y)
}

#' Hedge ratio term structure: hedge C1 exposure using C2…Cn
#'
#' Estimates the hedge ratio for hedging the front-month contract using
#' each deferred contract, computed over the full available history.
#'
#' @param wide_df          data.frame — wide prices
#' @param prefix           character
#' @param exposure_c       integer — contract being hedged (default 1)
#' @param max_hedge_c      integer — max deferred contract to try (default 12)
#' @param window           integer — rolling window for the estimate
#' @return data.frame: contract (int), beta, r_squared, basis_risk_pct
hedge_ratio_term_structure <- function(wide_df, prefix,
                                        exposure_c = 1L, max_hedge_c = 12L,
                                        window = 252L) {
  exp_col <- paste0(prefix, "_C", exposure_c)
  if (!exp_col %in% names(wide_df)) return(NULL)

  x_ret <- log(wide_df[[exp_col]] / dplyr::lag(wide_df[[exp_col]]))

  purrr::map_dfr(seq(exposure_c + 1L, max_hedge_c), function(cn) {
    hedge_col <- paste0(prefix, "_C", cn)
    if (!hedge_col %in% names(wide_df)) return(NULL)

    y_ret <- log(wide_df[[hedge_col]] / dplyr::lag(wide_df[[hedge_col]]))
    ok    <- !is.na(x_ret) & !is.na(y_ret)
    if (sum(ok) < window) return(NULL)

    fit  <- stats::lm(y_ret[ok] ~ x_ret[ok])
    smry <- summary(fit)

    data.frame(
      contract       = cn,
      beta           = stats::coef(fit)[2L],
      r_squared      = smry$r.squared,
      basis_risk_pct = smry$sigma / stats::sd(y_ret[ok]) * 100
    )
  })
}

#' Basis risk decomposition
#'
#' Decomposes total exposure risk into hedged risk (unhedgeable basis) and
#' hedged residual after applying the hedge ratio.
#'
#' @param x_ret      numeric vector — exposure returns
#' @param y_ret      numeric vector — hedge returns (same length)
#' @param hedge_ratio numeric scalar — the hedge ratio to apply
#' @return list:
#'   $unhedged_vol    — annualised vol of raw exposure
#'   $hedged_vol      — annualised vol after hedging
#'   $basis_risk_pct  — hedged_vol / unhedged_vol * 100 (% of risk remaining)
#'   $risk_reduction  — 1 - (hedged_vol / unhedged_vol) (fraction eliminated)
basis_risk_decomposition <- function(x_ret, y_ret, hedge_ratio) {
  ok           <- !is.na(x_ret) & !is.na(y_ret)
  hedged_ret   <- y_ret - hedge_ratio * x_ret

  unhedged_vol <- stats::sd(y_ret[ok])   * sqrt(TRADING_DAYS)
  hedged_vol   <- stats::sd(hedged_ret[ok]) * sqrt(TRADING_DAYS)

  list(
    unhedged_vol   = unhedged_vol,
    hedged_vol     = hedged_vol,
    basis_risk_pct = hedged_vol / unhedged_vol * 100,
    risk_reduction = 1 - (hedged_vol / unhedged_vol)
  )
}

#' Build rolling hedge ratio time series between two contracts
#'
#' Returns beta and r_squared as columns merged with the date column.
#'
#' @param wide_df    data.frame — wide prices
#' @param prefix     character
#' @param exp_c      integer — exposure contract
#' @param hedge_c    integer — hedge contract
#' @param window     integer
#' @return data.frame: date, beta, r_squared, ci_lower, ci_upper, resid_sd
rolling_hedge_ratio_series <- function(wide_df, prefix, exp_c, hedge_c, window = 63L) {
  exp_col   <- paste0(prefix, "_C", exp_c)
  hedge_col <- paste0(prefix, "_C", hedge_c)
  if (!all(c(exp_col, hedge_col) %in% names(wide_df))) return(NULL)

  x_ret <- log(wide_df[[exp_col]]   / dplyr::lag(wide_df[[exp_col]]))
  y_ret <- log(wide_df[[hedge_col]] / dplyr::lag(wide_df[[hedge_col]]))

  ratios <- ols_hedge_ratio(x_ret, y_ret, window)
  dplyr::bind_cols(dplyr::select(wide_df, date), ratios)
}

#' Rolling OLS hedge ratio across two different markets
#'
#' Accepts separate wide data frames for the exposure and hedge instruments.
#' Series are aligned on common dates before computing returns.
#'
#' @param exp_wide     data.frame — wide prices for the exposure market
#' @param exp_prefix   character  — RTL prefix for the exposure market
#' @param exp_c        integer    — exposure contract number
#' @param hedge_wide   data.frame — wide prices for the hedge market
#' @param hedge_prefix character  — RTL prefix for the hedge market
#' @param hedge_c      integer    — hedge contract number
#' @param window       integer    — rolling window in trading days
#' @return data.frame: date, beta, r_squared, ci_lower, ci_upper, resid_sd
rolling_hedge_ratio_cross_market <- function(exp_wide, exp_prefix, exp_c,
                                              hedge_wide, hedge_prefix, hedge_c,
                                              window = 63L) {
  exp_col   <- paste0(exp_prefix,   "_C", exp_c)
  hedge_col <- paste0(hedge_prefix, "_C", hedge_c)
  if (!exp_col   %in% names(exp_wide))   return(NULL)
  if (!hedge_col %in% names(hedge_wide)) return(NULL)

  exp_series   <- dplyr::select(exp_wide,   date, price = dplyr::all_of(exp_col))
  hedge_series <- dplyr::select(hedge_wide, date, price = dplyr::all_of(hedge_col))
  merged       <- dplyr::inner_join(exp_series, hedge_series,
                                    by = "date", suffix = c("_exp", "_hedge")) %>%
    dplyr::arrange(date)

  x_ret <- log(merged$price_exp   / dplyr::lag(merged$price_exp))
  y_ret <- log(merged$price_hedge / dplyr::lag(merged$price_hedge))

  ratios <- ols_hedge_ratio(x_ret, y_ret, window)
  dplyr::bind_cols(dplyr::select(merged, date), ratios)
}

#' Cross-market hedge ratio term structure
#'
#' Fixes the exposure market and contract, then estimates the OLS hedge ratio
#' against each deferred contract of the hedge market (full-history regression).
#'
#' @param exp_wide     data.frame — wide prices for the exposure market
#' @param exp_prefix   character
#' @param exp_c        integer    — exposure contract
#' @param hedge_wide   data.frame — wide prices for the hedge market
#' @param hedge_prefix character
#' @param max_hedge_c  integer    — max hedge contract to evaluate
#' @param window       integer    — minimum observation count required
#' @return data.frame: contract, beta, r_squared, basis_risk_pct
hedge_ratio_term_structure_cross_market <- function(exp_wide, exp_prefix, exp_c,
                                                     hedge_wide, hedge_prefix,
                                                     max_hedge_c = 12L,
                                                     window      = 252L) {
  exp_col <- paste0(exp_prefix, "_C", exp_c)
  if (!exp_col %in% names(exp_wide)) return(NULL)

  exp_series <- dplyr::select(exp_wide, date, exp = dplyr::all_of(exp_col))

  purrr::map_dfr(seq_len(max_hedge_c), function(cn) {
    hedge_col <- paste0(hedge_prefix, "_C", cn)
    if (!hedge_col %in% names(hedge_wide)) return(NULL)

    hedge_series <- dplyr::select(hedge_wide, date, hedge = dplyr::all_of(hedge_col))
    merged       <- dplyr::inner_join(exp_series, hedge_series, by = "date") %>%
      dplyr::arrange(date)

    x_ret <- log(merged$exp   / dplyr::lag(merged$exp))
    y_ret <- log(merged$hedge / dplyr::lag(merged$hedge))
    ok    <- !is.na(x_ret) & !is.na(y_ret)
    if (sum(ok) < window) return(NULL)

    fit  <- stats::lm(y_ret[ok] ~ x_ret[ok])
    smry <- summary(fit)

    data.frame(
      contract       = cn,
      beta           = stats::coef(fit)[2L],
      r_squared      = smry$r.squared,
      basis_risk_pct = smry$sigma / stats::sd(y_ret[ok]) * 100
    )
  })
}
