# app_server.R
# Top-level server: reactive data loading, shared state, module wiring.
#
# Data flow:
#   RTL::dflong  →  build_market_data()  →  mkt_data (reactiveVal)
#                                            passed to all feature modules
#
#   FRED DGS tickers  →  fetch_fred_zero_curve()  →  zero_curve_panel (reactiveVal)
#                                                      passed to mod_yield_curve_server

app_server <- function(input, output, session) {

  # ── 1. Load and cache commodity data ───────────────────────────────────────
  # build_market_data() reads RTL::dflong for the given market prefix and
  # returns list($long, $wide, $ret_wide). Computed once at startup.
  mkt_data <- shiny::reactiveVal(
    purrr::map(
      stats::setNames(ENABLED_MARKETS, ENABLED_MARKETS),
      function(mkt) {
        build_market_data(
          prefix        = MARKETS[[mkt]]$rtl_prefix,
          max_contracts = MARKETS[[mkt]]$max_contracts
        )
      }
    )
  )

  # ── 2. Fetch FRED zero curve at startup ────────────────────────────────────
  # Runs once when the session starts. Shows a notification while loading.
  # On failure (no internet, FRED down) falls back to an empty panel —
  # the Yield Curve tab will display a "no data" message gracefully.
  zero_curve_panel <- shiny::reactiveVal(
    data.frame(date = as.Date(character(0)),
               maturity = numeric(0),
               yield    = numeric(0))
  )

  shiny::observe({
    shiny::showNotification("Loading Treasury yield curve from FRED…",
                            id       = "fred_loading",
                            duration = NULL,
                            type     = "message")
    tryCatch({
      panel <- fetch_fred_zero_curve(from = "2000-01-01")
      zero_curve_panel(panel)
      shiny::removeNotification("fred_loading")
    }, error = function(e) {
      shiny::removeNotification("fred_loading")
      shiny::showNotification(
        paste("Could not load FRED data:", conditionMessage(e)),
        type     = "warning",
        duration = 8
      )
    })
  }) |> shiny::bindEvent(TRUE, once = TRUE)   # run exactly once at startup

  # ── 3. Wire feature modules ─────────────────────────────────────────────────
  mod_market_process_server("process")

  mod_forward_curve_server("fwd",    mkt_data = mkt_data)
  mod_volatility_server("vol",       mkt_data = mkt_data)
  mod_seasonality_server("season",   mkt_data = mkt_data)
  mod_hedge_ratios_server("hedge",   mkt_data = mkt_data)

  mod_yield_curve_server("yc",       zero_curve_panel = zero_curve_panel)

  mod_correlation_server("corr",     mkt_data = mkt_data)
}
