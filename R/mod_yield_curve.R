# mod_yield_curve.R
# Yield curve display using FRED zero curve data.
# Sidebar mirrors mod_forward_curve.R: date range for history panels,
# snapshot date for point-in-time curve views.

mod_yield_curve_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      dateRangeInput(
        ns("date_range"), "Date range",
        start = Sys.Date() - 365 * 5,
        end   = Sys.Date()
      ),
      actionButton(
        ns("full_history"), "Full history",
        class = "btn btn-sm btn-outline-secondary w-100 mb-2"
      ),
      hr(),
      dateInput(ns("snap_date"), "Snapshot date", value = Sys.Date() - 1L),
      dateInput(ns("comp_date"), "Compare to (optional)", value = NULL),
      hr(),
      checkboxInput(ns("show_forward"), "Overlay implied forward rates", value = FALSE),
      hr(),
      numericInput(
        ns("interp_mat"), "Interpolate yield at maturity (years)",
        value = 3, min = 0.1, max = 30, step = 0.25
      ),
      verbatimTextOutput(ns("interp_result"))
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Zero Curve",
        plotly::plotlyOutput(ns("zero_curve"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Yield History",
        plotly::plotlyOutput(ns("yield_history"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "2s10s Spread",
        plotly::plotlyOutput(ns("two_ten"), height = "70%", width = "100%")
      )
    )
  )
}

mod_yield_curve_server <- function(id, zero_curve_panel) {
  # zero_curve_panel: reactive data.frame(date, maturity, yield)
  moduleServer(id, function(input, output, session) {

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
                           start = as.Date("2000-01-01"),
                           end   = Sys.Date())
    })

    # Panel filtered to the selected date range (drives history panels)
    panel_ranged <- reactive({
      panel <- zero_curve_panel()
      req(panel, nrow(panel) > 0L)
      dplyr::filter(panel,
                    date >= input$date_range[1],
                    date <= input$date_range[2])
    }) %>% bindEvent(zero_curve_panel(), input$date_range)

    # Nearest available trading date helper
    nearest_date <- function(target, available) {
      if (is.na(target) || length(available) == 0L) return(as.Date(NA))
      available[which.min(abs(as.numeric(available - target)))]
    }

    # Snap user-entered snapshot date to nearest date in the full panel
    snap_date_actual <- reactive({
      panel <- zero_curve_panel()
      req(panel, nrow(panel) > 0L)
      nearest_date(input$snap_date, unique(panel$date))
    }) %>% bindEvent(zero_curve_panel(), input$snap_date)

    snap_spline <- reactive({
      d <- snap_date_actual()
      req(!is.na(d))
      get_date_spline(zero_curve_panel(), d)
    }) %>% bindEvent(snap_date_actual())

    # ── Interpolation sidebar output ─────────────────────────────────────────
    output$interp_result <- renderText({
      fn <- snap_spline()
      req(fn)
      y <- interpolate_zero_yield(fn, input$interp_mat)
      sprintf("%.2fyr zero yield: %.3f%%", input$interp_mat, y * 100)
    })

    # ── Zero Curve snapshot ──────────────────────────────────────────────────
    output$zero_curve <- plotly::renderPlotly({
      fn <- snap_spline()
      req(fn)
      d <- snap_date_actual()

      smooth_df <- interpolated_term_structure(fn, from = 0.25, to = 30, by = 0.25)
      knots     <- dplyr::filter(zero_curve_panel(), date == d)[, c("maturity", "yield")]

      p <- plotly::plot_ly() %>%
        plotly::add_lines(
          data = smooth_df, x = ~maturity, y = ~yield * 100,
          name = format(d, "%b %d, %Y"),
          line = list(color = "#1f77b4", width = 2.5),
          hovertemplate = "%{x:.2f}yr: %{y:.3f}%<extra></extra>"
        ) %>%
        plotly::add_markers(
          data = knots, x = ~maturity, y = ~yield * 100,
          name = "FRED knots", showlegend = FALSE,
          marker = list(color = "#1f77b4", size = 7),
          hovertemplate = "%{x:.2f}yr: %{y:.3f}%<extra></extra>"
        )

      if (isTRUE(input$show_forward)) {
        fwd_df <- dplyr::mutate(smooth_df,
          fwd = forward_rates(fn, maturity) * 100
        )
        p <- plotly::add_lines(p,
          data = fwd_df, x = ~maturity, y = ~fwd,
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

      p %>%
        plotly::layout(
          xaxis     = list(title = "Maturity (years)"),
          yaxis     = list(title = "Yield (%)"),
          legend    = list(orientation = "h"),
          hovermode = "x unified"
        )
    }) %>% bindEvent(snap_spline(), input$comp_date, input$show_forward)

    # ── Yield Curve History heatmap ──────────────────────────────────────────
    # x = date, y = maturity (1M bottom → 30Y top), colour = yield (%).
    # Inversion episodes show as red at the short end against a green long end.
    output$yield_history <- plotly::renderPlotly({
      hist_df <- panel_ranged()
      req(hist_df, nrow(hist_df) > 0L)

      mat_order <- c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "20Y", "30Y")
      mat_vals  <- c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3, 5, 7, 10, 20, 30)

      # Map each numeric maturity to the nearest label
      hist_labeled <- hist_df %>%
        dplyr::mutate(
          mat_label = mat_order[
            vapply(maturity,
                   function(m) which.min(abs(m - mat_vals)),
                   integer(1L))
          ]
        ) %>%
        dplyr::filter(mat_label %in% mat_order)

      # Pivot wide: rows = dates, cols = maturity labels
      hist_wide <- hist_labeled %>%
        dplyr::select(date, mat_label, yield) %>%
        tidyr::pivot_wider(
          names_from  = mat_label,
          values_from = yield,
          values_fn   = dplyr::first   # guard against any duplicates
        ) %>%
        dplyr::arrange(date)

      mats_present <- intersect(mat_order, names(hist_wide))
      req(length(mats_present) >= 2L)

      # Transpose: heatmap needs rows = maturities, cols = dates
      z_pct <- t(as.matrix(hist_wide[, mats_present])) * 100

      plotly::plot_ly(
        x          = hist_wide$date,
        y          = mats_present,
        z          = z_pct,
        type       = "heatmap",
        colorscale = list(c(0, "#c0392b"), c(0.4, "#f5f5f5"), c(1, "#1a7340")),
        zmin       = 0,
        zmax       = 6,
        colorbar   = list(title = "Yield (%)"),
        hovertemplate = "%{x|%Y-%m-%d} %{y}: %{z:.2f}%<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Maturity",
                       categoryorder = "array",
                       categoryarray = mats_present)
        )
    }) %>% bindEvent(panel_ranged())

    # ── 2s10s Spread (filtered to date range) ────────────────────────────────
    output$two_ten <- plotly::renderPlotly({
      hist_df <- panel_ranged()
      req(hist_df, nrow(hist_df) > 0L)

      y2_df  <- dplyr::filter(hist_df, maturity == 2)[,  c("date", "yield")]
      y10_df <- dplyr::filter(hist_df, maturity == 10)[, c("date", "yield")]
      names(y2_df)[2]  <- "y2"
      names(y10_df)[2] <- "y10"

      spread_df <- dplyr::inner_join(y2_df, y10_df, by = "date") %>%
        dplyr::mutate(spread = (y10 - y2) * 100)

      pos_df <- dplyr::filter(spread_df, spread >= 0)
      neg_df <- dplyr::filter(spread_df, spread <  0)

      plotly::plot_ly() %>%
        plotly::add_bars(
          data = pos_df, x = ~date, y = ~spread,
          name = "Normal (10y > 2y)", marker = list(color = "#1a7340"),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}bp<extra></extra>"
        ) %>%
        plotly::add_bars(
          data = neg_df, x = ~date, y = ~spread,
          name = "Inverted (10y < 2y)", marker = list(color = "#c0392b"),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}bp<extra></extra>"
        ) %>%
        plotly::layout(
          barmode = "relative",
          xaxis   = list(title = ""),
          yaxis   = list(title = "2s10s Spread (%)"),
          legend  = list(orientation = "h")
        )
    }) %>% bindEvent(panel_ranged())
  })
}
