# mod_forward_curve.R
# Forward curve analysis: snapshot, regime classification, roll yield, history.

mod_forward_curve_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel"), show_window = FALSE, show_contracts = FALSE),
      hr(),
      dateInput(ns("snap_date"), "Snapshot date", value = Sys.Date() - 1L),
      dateInput(ns("comp_date"), "Compare to (optional)", value = NULL),
      numericInput(ns("back_c"), "Back contract (slope)", value = 12L, min = 2L, max = 36L)
    ),
    # ── Value boxes ──────────────────────────────────────────────────────────
    bslib::layout_column_wrap(
      width = 1/3,
      bslib::value_box(
        title    = "Curve Regime",
        value    = textOutput(ns("regime_label"), inline = TRUE),
        showcase = bsicons::bs_icon("bar-chart-steps"),
        id       = ns("regime_box")
      ),
      bslib::value_box(
        title    = "C1–C12 Slope ($/unit)",
        value    = textOutput(ns("slope_label"), inline = TRUE),
        showcase = bsicons::bs_icon("arrows-expand")
      ),
      bslib::value_box(
        title    = "Roll Yield (annualised)",
        value    = textOutput(ns("roll_label"), inline = TRUE),
        showcase = bsicons::bs_icon("arrow-repeat")
      )
    ),
    # ── Charts ───────────────────────────────────────────────────────────────
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Curve Snapshot",
        plotly::plotlyOutput(ns("curve_snapshot"), height = "420px")
      ),
      bslib::nav_panel(
        "Curve History (Fan)",
        plotly::plotlyOutput(ns("fwd_fan"), height = "500px")
      ),
      bslib::nav_panel(
        "Regime History",
        plotly::plotlyOutput(ns("regime_history"), height = "420px")
      ),
      bslib::nav_panel(
        "Roll Yield",
        plotly::plotlyOutput(ns("roll_yield_plot"), height = "420px")
      )
    )
  )
}

mod_forward_curve_server <- function(id, mkt_data) {
  # mkt_data: reactive list with $wide per market key (e.g., mkt_data()$CL$wide)
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    # ── Reactive: filtered wide data for selected market ─────────────────────
    wide <- reactive({
      m <- sel$market()
      req(m, mkt_data()[[m]])
      filter_date_range(mkt_data()[[m]]$wide,
                        sel$date_from(), sel$date_to())
    }) |> bindEvent(sel$market(), sel$date_from(), sel$date_to())

    prefix <- reactive(MARKETS[[sel$market()]]$rtl_prefix)
    unit   <- reactive(MARKETS[[sel$market()]]$unit)

    # ── Snapshot metrics ─────────────────────────────────────────────────────
    snap_date <- reactive({
      dates <- wide()$date
      if (length(dates) == 0L) return(as.Date(NA)) # no data in range → NA
      d <- input$snap_date
      if (is.na(d)) return(max(dates))             # default to latest date
      dates[which.min(abs(as.numeric(dates - d)))] # snap to nearest trading day
    })

    slope <- reactive({
      req(!is.na(snap_date()), isTruthy(input$back_c))
      compute_curve_slope(wide(), prefix(), snap_date(), back = input$back_c)
    })

    regime <- reactive({
      req(!is.na(snap_date()))
      classify_curve_shape(slope())
    })

    roll_yld_last <- reactive({
      ry <- compute_roll_yield(wide(), prefix())
      if (is.null(ry) || nrow(ry) == 0L) return(NA_real_)
      tail(ry$roll_yield, 1L)
    })

    # ── Value box outputs ─────────────────────────────────────────────────────
    output$regime_label <- renderText(regime())
    output$slope_label  <- renderText({
      s <- slope()
      if (is.na(s)) "N/A" else sprintf("%+.2f %s", s, unit())
    })
    output$roll_label   <- renderText({
      r <- roll_yld_last()
      if (is.na(r)) "N/A" else scales::percent(r, accuracy = 0.1)
    })

    # ── Curve Snapshot ────────────────────────────────────────────────────────
    output$curve_snapshot <- plotly::renderPlotly({
      req(!is.na(snap_date()))
      snap <- build_curve_snapshot(wide(), prefix(), snap_date())
      req(snap)

      p <- plotly::plot_ly() |>
        plotly::add_lines(
          data = snap, x = ~contract, y = ~price,
          name = format(snap_date(), "%b %d, %Y"),
          line = list(color = MARKETS[[sel$market()]]$color, width = 2.5),
          hovertemplate = "C%{x}: %{y:.2f}<extra></extra>"
        )

      # Optional comparison date overlay
      comp_d <- input$comp_date
      if (isTruthy(comp_d)) {
        comp_snap <- build_curve_snapshot(wide(), prefix(), comp_d)
        if (!is.null(comp_snap)) {
          p <- p |>
            plotly::add_lines(
              data = comp_snap, x = ~contract, y = ~price,
              name = format(comp_d, "%b %d, %Y"),
              line = list(color = "#adb5bd", width = 2, dash = "dash"),
              hovertemplate = "C%{x}: %{y:.2f}<extra></extra>"
            )
        }
      }

      p |>
        plotly::layout(
          xaxis  = list(title = "Contract", dtick = 3),
          yaxis  = list(title = unit()),
          legend = list(orientation = "h"),
          hovermode = "x unified"
        )
    })

    # ── Regime History ────────────────────────────────────────────────────────
    output$regime_history <- plotly::renderPlotly({
      req(isTruthy(input$back_c))
      hist <- curve_slope_history(wide(), prefix(), back = input$back_c)
      req(hist)

      fracs <- regime_fractions_by_year(hist)

      regime_colors <- c(
        "Steep Backwardation" = "#1a7340",
        "Mild Backwardation"  = "#52a36b",
        "Flat"                = "#adb5bd",
        "Mild Contango"       = "#e07b39",
        "Steep Contango"      = "#c0392b"
      )

      plotly::plot_ly(fracs,
        x    = ~year, y = ~fraction, color = ~regime,
        colors = regime_colors,
        type = "bar",
        hovertemplate = "%{y:.1%}<extra>%{fullData.name}</extra>"
      ) |>
        plotly::layout(
          barmode = "stack",
          xaxis   = list(title = "Year"),
          yaxis   = list(title = "Fraction of trading days", tickformat = ".0%"),
          legend  = list(orientation = "h", y = -0.15)
        )
    })

    # ── Curve History (Fan) ───────────────────────────────────────────────────
    # Plots the C1 spot price over time as the main spine, then draws forward
    # curves at sampled dates as semi-transparent "ribs" branching off the
    # spine. Each rib anchors C1 at the snapshot date and plots C2–C12 at
    # their approximate delivery dates (snapshot_date + (N-1) months).
    output$fwd_fan <- plotly::renderPlotly({
      w   <- wide()
      pfx <- prefix()
      u   <- unit()
      req(w, nrow(w) > 50L)

      c1_col <- paste0(pfx, "_C1")
      req(c1_col %in% names(w))

      # C1 price time series — the main spine of the chart
      c1_ts <- dplyr::select(w, date, c1 = dplyr::all_of(c1_col)) |>
        dplyr::filter(!is.na(c1))
      req(nrow(c1_ts) > 2L)

      # Adaptive sampling: at most 30 ribs regardless of date range
      step       <- max(63L, nrow(c1_ts) %/% 30L)
      sample_idx <- seq(1L, nrow(c1_ts), by = step)
      # Always include the most recent date
      if (utils::tail(sample_idx, 1L) != nrow(c1_ts)) {
        sample_idx <- c(sample_idx, nrow(c1_ts))
      }
      sampled_dates <- c1_ts$date[sample_idx]

      # Build forward curve snapshots at each sampled date.
      # Delivery date for contract N at snapshot date d: d + (N-1) * ~30.44 days.
      fan_df <- purrr::map_dfr(sampled_dates, function(d) {
        snap <- build_curve_snapshot(w, pfx, d, max_c = 12L)
        if (is.null(snap) || nrow(snap) == 0L) return(NULL)
        dplyr::mutate(snap,
          snap_date     = d,
          delivery_date = d + lubridate::days(as.integer(round((contract - 1L) * 30.44)))
        )
      })
      req(!is.null(fan_df), nrow(fan_df) > 0L)

      mkt_color <- MARKETS[[sel$market()]]$color

      # Convert hex color to rgba for semi-transparent historical ribs
      hex <- sub("^#", "", mkt_color)
      r   <- strtoi(substr(hex, 1L, 2L), 16L)
      g   <- strtoi(substr(hex, 3L, 4L), 16L)
      b   <- strtoi(substr(hex, 5L, 6L), 16L)
      rib_rgba <- sprintf("rgba(%d,%d,%d,0.18)", r, g, b)

      # Historical ribs combined into one trace with NA row breaks between curves
      hist_dates <- sampled_dates[-length(sampled_dates)]
      rib_rows   <- purrr::map_dfr(hist_dates, function(d) {
        seg <- dplyr::filter(fan_df, snap_date == d, !is.na(price)) |>
          dplyr::arrange(contract)
        if (nrow(seg) < 2L) return(NULL)
        dplyr::bind_rows(
          dplyr::select(seg, delivery_date, price),
          data.frame(delivery_date = as.Date(NA), price = NA_real_)
        )
      })

      # Most recent rib — highlighted in full market color
      latest_date  <- max(sampled_dates)
      latest_curve <- dplyr::filter(fan_df, snap_date == latest_date, !is.na(price)) |>
        dplyr::arrange(contract)

      mkt_label <- MARKETS[[sel$market()]]$label

      p <- plotly::plot_ly() |>
        plotly::add_lines(
          data = c1_ts, x = ~date, y = ~c1,
          name = paste0(pfx, "01 (front month)"),
          line = list(color = mkt_color, width = 2),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}<extra></extra>"
        )

      if (!is.null(rib_rows) && nrow(rib_rows) > 0L) {
        p <- plotly::add_lines(p,
          data        = rib_rows, x = ~delivery_date, y = ~price,
          name        = "Forward curves (sampled)",
          line        = list(color = rib_rgba, width = 1),
          connectgaps = FALSE,
          hoverinfo   = "skip"
        )
      }

      if (nrow(latest_curve) > 0L) {
        p <- plotly::add_lines(p,
          data          = latest_curve, x = ~delivery_date, y = ~price,
          name          = glue::glue("Forward curve ({format(latest_date, '%b %d, %Y')})"),
          line          = list(color = mkt_color, width = 2.5, dash = "dash"),
          customdata    = ~contract,
          hovertemplate = "C%{customdata}: %{y:.2f}<extra></extra>"
        )
      }

      p |>
        plotly::layout(
          xaxis     = list(title = "Date / Approximate Delivery Month"),
          yaxis     = list(title = glue::glue("{u} \u2014 {mkt_label}")),
          legend    = list(orientation = "h"),
          hovermode = "x unified"
        )
    }) |> bindEvent(wide())

    # ── Roll Yield ─────────────────────────────────────────────────────────────
    output$roll_yield_plot <- plotly::renderPlotly({
      ry <- compute_roll_yield(wide(), prefix())
      req(ry)

      plotly::plot_ly(ry, x = ~date, y = ~roll_yield, type = "scatter", mode = "lines",
        line = list(color = MARKETS[[sel$market()]]$color, width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.1%}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis  = list(title = ""),
          yaxis  = list(title = "Annualised roll yield (C1\u2192C2)", tickformat = ".1%"),
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1,
            y0 = 0, y1 = 0,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )
    })
  })
}
