# fct_eia_data.R
# EIA fundamental data: series definitions, fetching via RTL::eia2tidy_all,
# feather cache loading, and the 5-year range analytics helper.
#
# Fetch flow:
#   build_eia_fundamentals()  ← called by dev/fetch_eia_data.R (GitHub Actions nightly)
#   load_eia_data()           ← called once at Shiny startup

# ── Series definitions ────────────────────────────────────────────────────────
# Each row defines one EIA weekly series.
# ticker:      EIA API v1 series ID (format used by RTL::eia2tidy_all)
# name:        unique short key used to join metadata after the API call
# commodity:   one of "crude", "distillate", "gasoline", "natural_gas"
# series_type: one of "stocks", "production", "imports", "exports", "storage"
# area:        short code used for filtering in the Shiny app
# area_label:  display label shown to the user
# units:       display unit string
#
# NOTE: Natural gas regional series IDs (NW2_EPG0_SWO_R3*_BCF.W) should be
# verified against the EIA API browser if regional storage data is missing.
# The Lower 48 total (R48) is well-established; regionals are best-effort.
EIA_TICKERS <- tibble::tribble(
  ~ticker,                              ~name,                  ~commodity,   ~series_type,  ~area,          ~area_label,                  ~units,
  # Crude oil ending stocks (Mbbl)
  "PET.WCRSTUS1.W",                     "crude_stocks_US",      "crude",      "stocks",      "US",           "U.S.",                       "Mbbl",
  "PET.WCRSTP11.W",                     "crude_stocks_PADD1",   "crude",      "stocks",      "PADD1",        "PADD 1 (East Coast)",        "Mbbl",
  "PET.WCRSTP21.W",                     "crude_stocks_PADD2",   "crude",      "stocks",      "PADD2",        "PADD 2 (Midwest)",           "Mbbl",
  "PET.WCRSTP31.W",                     "crude_stocks_PADD3",   "crude",      "stocks",      "PADD3",        "PADD 3 (Gulf Coast)",        "Mbbl",
  "PET.WCRSTP41.W",                     "crude_stocks_PADD4",   "crude",      "stocks",      "PADD4",        "PADD 4 (Rockies)",           "Mbbl",
  "PET.WCRSTP51.W",                     "crude_stocks_PADD5",   "crude",      "stocks",      "PADD5",        "PADD 5 (West Coast)",        "Mbbl",
  "PET.W_EPC0_SAX_YCUOK_MBBL.W",       "crude_stocks_Cushing", "crude",      "stocks",      "Cushing",      "Cushing, OK",                "Mbbl",
  # Crude production, imports, exports (Mbbl/d)
  "PET.WCRFPUS2.W",                     "crude_production_US",  "crude",      "production",  "US",           "U.S.",                       "Mbbl/d",
  "PET.WCRIMUS2.W",                     "crude_imports_US",     "crude",      "imports",     "US",           "U.S.",                       "Mbbl/d",
  "PET.WCREXUS2.W",                     "crude_exports_US",     "crude",      "exports",     "US",           "U.S.",                       "Mbbl/d",
  # Distillate fuel oil ending stocks (Mbbl)
  "PET.WDISTUS1.W",                     "dist_stocks_US",       "distillate", "stocks",      "US",           "U.S.",                       "Mbbl",
  "PET.WDISTP11.W",                     "dist_stocks_PADD1",    "distillate", "stocks",      "PADD1",        "PADD 1 (East Coast)",        "Mbbl",
  "PET.WDISTP21.W",                     "dist_stocks_PADD2",    "distillate", "stocks",      "PADD2",        "PADD 2 (Midwest)",           "Mbbl",
  "PET.WDISTP31.W",                     "dist_stocks_PADD3",    "distillate", "stocks",      "PADD3",        "PADD 3 (Gulf Coast)",        "Mbbl",
  "PET.WDISTP41.W",                     "dist_stocks_PADD4",    "distillate", "stocks",      "PADD4",        "PADD 4 (Rockies)",           "Mbbl",
  "PET.WDISTP51.W",                     "dist_stocks_PADD5",    "distillate", "stocks",      "PADD5",        "PADD 5 (West Coast)",        "Mbbl",
  # Total motor gasoline ending stocks (Mbbl)
  "PET.WGTSTUS1.W",                     "gas_stocks_US",        "gasoline",   "stocks",      "US",           "U.S.",                       "Mbbl",
  "PET.WGTSTP11.W",                     "gas_stocks_PADD1",     "gasoline",   "stocks",      "PADD1",        "PADD 1 (East Coast)",        "Mbbl",
  "PET.WGTSTP21.W",                     "gas_stocks_PADD2",     "gasoline",   "stocks",      "PADD2",        "PADD 2 (Midwest)",           "Mbbl",
  "PET.WGTSTP31.W",                     "gas_stocks_PADD3",     "gasoline",   "stocks",      "PADD3",        "PADD 3 (Gulf Coast)",        "Mbbl",
  "PET.WGTSTP41.W",                     "gas_stocks_PADD4",     "gasoline",   "stocks",      "PADD4",        "PADD 4 (Rockies)",           "Mbbl",
  "PET.WGTSTP51.W",                     "gas_stocks_PADD5",     "gasoline",   "stocks",      "PADD5",        "PADD 5 (West Coast)",        "Mbbl",
  # Natural gas working underground storage (Bcf)
  "NG.NW2_EPG0_SWO_R48_BCF.W",         "ng_storage_US",        "natural_gas","storage",     "US",           "U.S. Lower 48",              "Bcf",
  "NG.NW2_EPG0_SWO_R31_BCF.W",         "ng_storage_East",      "natural_gas","storage",     "East",         "East Region",                "Bcf",
  "NG.NW2_EPG0_SWO_R32_BCF.W",         "ng_storage_Midwest",   "natural_gas","storage",     "Midwest",      "Midwest Region",             "Bcf",
  "NG.NW2_EPG0_SWO_R33_BCF.W",         "ng_storage_Mountain",  "natural_gas","storage",     "Mountain",     "Mountain Region",            "Bcf",
  "NG.NW2_EPG0_SWO_R34_BCF.W",         "ng_storage_Pacific",   "natural_gas","storage",     "Pacific",      "Pacific Region",             "Bcf",
  "NG.NW2_EPG0_SWO_R35_BCF.W",         "ng_storage_SouthCent", "natural_gas","storage",     "SouthCentral", "South Central Region",       "Bcf"
)

# ── Fetch all series ──────────────────────────────────────────────────────────

#' Fetch all EIA fundamental series via RTL::eia2tidy_all and attach metadata
#'
#' Returns a tidy long data frame ready to write to the feather cache.
#'
#' @param api_key character — EIA API key (default: EIA_API_KEY env var)
#' @return data.frame with columns: date, commodity, series_type, area, area_label, units, value
build_eia_fundamentals <- function(api_key = Sys.getenv("EIA_API_KEY")) {
  if (nchar(api_key) == 0L) stop("EIA_API_KEY is not set.")

  tickers_input <- dplyr::select(EIA_TICKERS, ticker, name)

  message("Calling RTL::eia2tidy_all for ", nrow(tickers_input), " series...")
  raw <- RTL::eia2tidy_all(tickers = tickers_input, key = api_key, long = TRUE)
  # raw columns: date, series (= name), value

  # Metadata lookup keyed by name
  meta <- dplyr::select(EIA_TICKERS, name, commodity, series_type, area, area_label, units)

  raw %>%
    dplyr::rename(name = series) %>%
    dplyr::left_join(meta, by = "name") %>%
    dplyr::filter(!is.na(value), !is.na(commodity)) %>%
    dplyr::select(date, commodity, series_type, area, area_label, units, value) %>%
    dplyr::arrange(commodity, series_type, area, date)
}

# ── App-side loader ───────────────────────────────────────────────────────────

#' Resolve path to the feather cache file
eia_data_path <- function() {
  pkg <- tryCatch(
    system.file("extdata/eia_fundamentals.feather", package = "fin452golem"),
    error = function(e) ""
  )
  if (nchar(pkg) > 0L && file.exists(pkg)) return(pkg)
  dev <- "inst/extdata/eia_fundamentals.feather"
  if (file.exists(dev)) return(dev)
  NULL
}

#' Load cached EIA fundamentals from the feather file written by the nightly fetch
#'
#' Returns a list:
#'   $data         — data.frame, or NULL if the file is missing
#'   $last_updated — character timestamp ("YYYY-MM-DD HH:MM:SS UTC"), or NULL
#'   $is_stale     — TRUE when the file is absent or older than `stale_days`
#'
#' @param stale_days numeric — staleness threshold in days (default 3)
load_eia_data <- function(stale_days = 3) {
  path <- eia_data_path()
  if (is.null(path)) {
    return(list(data = NULL, last_updated = NULL, is_stale = TRUE))
  }

  data <- tryCatch(
    arrow::read_feather(path),
    error = function(e) { warning("Cannot read EIA feather: ", conditionMessage(e)); NULL }
  )

  ts_path      <- sub("\\.feather$", "_last_updated.txt", path)
  last_updated <- if (file.exists(ts_path)) readLines(ts_path, warn = FALSE)[1L] else NULL

  is_stale <- tryCatch({
    if (is.null(last_updated)) return(TRUE)
    lu <- as.POSIXct(last_updated, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    as.numeric(difftime(Sys.time(), lu, units = "days")) > stale_days
  }, error = function(e) TRUE)

  list(data = data, last_updated = last_updated, is_stale = is_stale)
}

# ── Analytics helper ──────────────────────────────────────────────────────────

#' Compute 5-year historical range bands for a weekly inventory series
#'
#' Groups by ISO week number. Returns the current year's data joined with
#' prior-5-year min, max, and mean for the same week — the standard EIA
#' inventory chart format.
#'
#' @param df           data.frame with columns `date` (Date) and `value` (numeric)
#' @param current_year integer — year to treat as "current" (default: latest in data)
#' @return data.frame: date, week, value, range_min, range_max, avg_5yr
compute_5yr_range <- function(df, current_year = NULL) {
  df <- dplyr::filter(df, !is.na(value)) %>%
    dplyr::mutate(year = lubridate::year(date),
                  week = lubridate::isoweek(date))

  if (is.null(current_year)) current_year <- max(df$year)
  ref_years <- seq(current_year - 5L, current_year - 1L)

  ref <- df %>%
    dplyr::filter(year %in% ref_years) %>%
    dplyr::group_by(week) %>%
    dplyr::summarise(
      range_min = min(value,  na.rm = TRUE),
      range_max = max(value,  na.rm = TRUE),
      avg_5yr   = mean(value, na.rm = TRUE),
      .groups   = "drop"
    )

  df %>%
    dplyr::filter(year == current_year) %>%
    dplyr::select(date, week, value) %>%
    dplyr::left_join(ref, by = "week")
}
