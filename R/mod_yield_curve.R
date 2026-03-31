# mod_yield_curve.R
# Yield curve display using user-provided FRED zero curve data.
# Also surfaces the cubic spline interpolation from fct_fred_data.R.
#
# The user provides a reactive `zero_curve_panel` data.frame with columns:
#   date (Date), maturity (numeric, years), yield (numeric, decimal)

mod_yield_curve_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      dateInput(ns("curve_date"), "Curve date", value = Sys.Date() - 1L),
      dateInput(ns("comp_date"),  "Compare to (optional)", value = NULL),
      hr(),
      checkboxInput(ns("show_forward"), "Overlay implied forward rates", value = FALSE),
      checkboxInput(ns("show_par"),     "Overlay par yields", value = FALSE),
      hr(),
      numericInput(ns("interp_mat"), "Interpolate yield at maturity (years)",
                   value = 3, min = 0.1, max = 30, step = 0.25),
      verbatimTextOutput(ns("interp_result"))
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Zero Curve",
        plotly::plotlyOutput(ns("zero_curve"), height = "420px")
      ),
      bslib::nav_panel(
        "2s10s Spread",
        plotly::plotlyOutput(ns("two_ten"), height = "420px")
      )
    )
  )
}

mod_yield_curve_server <- function(id, zero_curve_panel) {
  # zero_curve_panel: reactive data.frame(date, maturity, yield)
  moduleServer(id, function(input, output, session) {

    # Nearest available date helper
    nearest_date <- function(target, available) {
      if (is.na(target) || length(available) == 0L) return(NA)
      available[which.min(abs(as.numeric(available - target)))]
    }

    snap_spline <- reactive({
      panel <- zero_curve_panel()
      req(panel, nrow(panel) > 0L)
      d <- nearest_date(input$curve_date, unique(panel$date))
      req(!is.na(d))
      get_date_spline(panel, d)
    }) |> bindEvent(zero_curve_panel(), input$curve_date)

    # ── Interpolation output ──────────────────────────────────────────────────
    output$interp_result <- renderText({
      fn <- snap_spline()
      req(fn)
      y <- interpolate_zero_yield(fn, input$interp_mat)
      sprintf("%.4f yr zero yield: %.3f%%", input$interp_mat, y * 100)
    })

    # ── Zero Curve chart ──────────────────────────────────────────────────────
    output$zero_curve <- plotly::renderPlotly({
      fn <- snap_spline()
      req(fn)

      smooth_df <- interpolated_term_structure(fn, from = 0.25, to = 30, by = 0.25)

      # Raw knot points from panel
      panel <- zero_curve_panel()
      d     <- nearest_date(input$curve_date, unique(panel$date))
      knots <- panel[panel$date == d, c("maturity", "yield")]

      p <- plotly::plot_ly() |>
        plotly::add_lines(
          data = smooth_df, x = ~maturity, y = ~yield * 100,
          name = format(d, "%b %d, %Y"),
          line = list(color = "#1f77b4", width = 2.5),
          hovertemplate = "%{x:.2f}yr: %{y:.3f}%<extra></extra>"
        ) |>
        plotly::add_markers(
          data = knots, x = ~maturity, y = ~yield * 100,
          name = "FRED knots",
          marker = list(color = "#1f77b4", size = 7),
          hovertemplate = "%{x:.2f}yr: %{y:.3f}%<extra></extra>"
        )

      if (isTRUE(input$show_forward)) {
        fwd_df <- smooth_df
        fwd_df$forward <- forward_rates(fn, fwd_df$maturity) * 100
        p <- plotly::add_lines(p,
          data = fwd_df, x = ~maturity, y = ~forward,
          name = "Implied forward rates",
          line = list(color = "#ff7f0e", width = 1.5, dash = "dash"),
          hovertemplate = "%{x:.2f}yr fwd: %{y:.3f}%<extra></extra>"
        )
      }

      if (isTruthy(input$comp_date)) {
        comp_d  <- nearest_date(input$comp_date, unique(zero_curve_panel()$date))
        comp_fn <- get_date_spline(zero_curve_panel(), comp_d)
        if (!is.null(comp_fn)) {
          comp_smooth <- interpolated_term_structure(comp_fn)
          p <- plotly::add_lines(p,
            data = comp_smooth, x = ~maturity, y = ~yield * 100,
            name = format(comp_d, "%b %d, %Y"),
            line = list(color = "#adb5bd", width = 1.5, dash = "dash"),
            hovertemplate = "%{x:.2f}yr: %{y:.3f}%<extra></extra>"
          )
        }
      }

      p |>
        plotly::layout(
          xaxis = list(title = "Maturity (years)"),
          yaxis = list(title = "Yield (%)"),
          legend = list(orientation = "h"),
          hovermode = "x unified"
        )
    })

    # ── 2s10s Spread ──────────────────────────────────────────────────────────
    output$two_ten <- plotly::renderPlotly({
      panel <- zero_curve_panel()
      req(panel, nrow(panel) > 0L)

      # Extract 2yr and 10yr directly from panel rows (nearest available maturity)
      # DGS2 → maturity 2, DGS10 → maturity 10 are always present in FRED pull
      y2_df  <- panel[panel$maturity == 2,  c("date", "yield")]
      y10_df <- panel[panel$maturity == 10, c("date", "yield")]
      names(y2_df)[2]  <- "y2"
      names(y10_df)[2] <- "y10"

      spread_df <- dplyr::inner_join(y2_df, y10_df, by = "date")
      spread_df <- dplyr::mutate(spread_df, spread = (y10 - y2) * 100)

      pos_df <- dplyr::filter(spread_df, spread >= 0)
      neg_df <- dplyr::filter(spread_df, spread <  0)

      plotly::plot_ly() |>
        plotly::add_bars(
          data = pos_df, x = ~date, y = ~spread,
          name = "Normal (10y > 2y)", marker = list(color = "#1a7340"),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}bp<extra></extra>"
        ) |>
        plotly::add_bars(
          data = neg_df, x = ~date, y = ~spread,
          name = "Inverted (10y < 2y)", marker = list(color = "#c0392b"),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}bp<extra></extra>"
        ) |>
        plotly::layout(
          barmode = "relative",
          xaxis   = list(title = ""),
          yaxis   = list(title = "2s10s Spread (%)"),
          legend  = list(orientation = "h")
        )
    }) |> bindEvent(zero_curve_panel())
  })
}
