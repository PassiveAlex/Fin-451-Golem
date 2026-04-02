# app_ui.R
# Top-level UI assembly using bslib::page_navbar.

app_ui <- function() {
  bslib::page_navbar(
    title = shiny::span(
      bsicons::bs_icon("bar-chart-line-fill"),
      " Energy Risk Dashboard"
    ),
    theme     = APP_THEME,
    fillable  = TRUE,
    id        = "main_nav",

    # Include custom CSS and shinyjs
    shiny::tags$head(
      shiny::includeCSS(app_sys("app/www/custom.css"))
    ),
    shinyjs::useShinyjs(),

    # ── Market Process ──────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("diagram-3"), " Market Process"),
      value = "market_process",
      mod_market_process_ui("process")
    ),

    # ── Forward Curve ───────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("graph-up"), " Forward Curve"),
      value = "forward_curve",
      mod_forward_curve_ui("fwd")
    ),

    # ── Volatility ──────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("activity"), " Volatility"),
      value = "volatility",
      mod_volatility_ui("vol")
    ),

    # ── Seasonality ─────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("calendar3"), " Seasonality"),
      value = "seasonality",
      mod_seasonality_ui("season")
    ),

    # ── Hedge Ratios ─────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("shield-check"), " Hedge Ratios"),
      value = "hedge_ratios",
      mod_hedge_ratios_ui("hedge")
    ),

    # ── Yield Curve ─────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("bank"), " Yield Curve"),
      value = "yield_curve",
      mod_yield_curve_ui("yc")
    ),

    # ── Correlations ─────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = shiny::span(bsicons::bs_icon("grid"), " Correlations"),
      value = "correlations",
      mod_correlation_ui("corr")
    ),

    bslib::nav_spacer()
  )
}
