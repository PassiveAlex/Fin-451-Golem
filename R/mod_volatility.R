# mod_volatility.R
# Volatility: realized vol term structure, rolling vol over time, vol cone.

mod_volatility_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel")),
      hr(),
      checkboxGroupInput(
        ns("contracts"),
        label    = "Contracts for time series",
        choices  = c("C1" = 1L, "C3" = 3L, "C6" = 6L, "C12" = 12L),
        selected = c(1L, 3L, 6L)
      )
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Vol Term Structure",
        plotly::plotlyOutput(ns("vol_ts"), height = "420px")
      ),
      bslib::nav_panel(
        "Vol Over Time",
        plotly::plotlyOutput(ns("vol_history"), height = "420px")
      ),
      bslib::nav_panel(
        "Vol Cone",
        plotly::plotlyOutput(ns("vol_cone"), height = "420px")
      )
    )
  )
}

mod_volatility_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    wide <- reactive({
      m <- sel$market()
      req(m, mkt_data()[[m]])
      filter_date_range(mkt_data()[[m]]$wide,
                        sel$date_from(), sel$date_to())
    }) |> bindEvent(sel$market(), sel$date_from(), sel$date_to())

    prefix <- reactive(MARKETS[[sel$market()]]$rtl_prefix)
    unit   <- reactive(MARKETS[[sel$market()]]$unit)

    # Update contract checkboxes when market changes
    observeEvent(sel$market(), {
      mkt      <- sel$market()
      max_c    <- MARKETS[[mkt]]$max_contracts
      # Build choices up to max_contracts for this market
      possible <- c(1L, 3L, 6L, 12L)
      choices  <- possible[possible <= max_c]
      nms      <- paste0("C", choices)
      updateCheckboxGroupInput(
        session, "contracts",
        choices  = stats::setNames(choices, nms),
        selected = choices[choices <= min(6L, max_c)]
      )
    }, ignoreInit = TRUE)

    # ── Vol Term Structure snapshot ───────────────────────────────────────────
    output$vol_ts <- plotly::renderPlotly({
      snap_date <- max(wide()$date, na.rm = TRUE)
      vts       <- vol_term_structure_snapshot(wide(), prefix(), snap_date,
                                                window = sel$window())
      req(vts)

      plotly::plot_ly(vts, x = ~contract, y = ~vol,
        type = "bar",
        marker = list(color = MARKETS[[sel$market()]]$color),
        hovertemplate = "C%{x}: %{y:.1%} ann. vol<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = "Contract", dtick = 3),
          yaxis = list(title = glue::glue("{sel$window()}d Ann. Realised Vol"),
                       tickformat = ".0%")
        )
    }) |> bindEvent(wide(), sel$window())

    # ── Vol Over Time ─────────────────────────────────────────────────────────
    output$vol_history <- plotly::renderPlotly({
      cnts <- as.integer(input$contracts)
      req(length(cnts) > 0L)

      vol_df <- rolling_vol_wide(wide(), prefix(), cnts, window = sel$window())

      # Identify high-vol regime for background shading
      c1_col <- paste0("vol_", prefix(), "_C1")
      regime_flag <- if (c1_col %in% names(vol_df)) {
        vol_regime_high(vol_df[[c1_col]])
      } else rep(FALSE, nrow(vol_df))

      # Build background shading shapes for regime periods
      run_start <- which(diff(c(FALSE, regime_flag)) == 1L)
      run_end   <- which(diff(c(regime_flag, FALSE)) == -1L)
      shapes <- purrr::map2(run_start, run_end, function(s, e) {
        list(type = "rect", xref = "x", yref = "paper",
             x0 = vol_df$date[s], x1 = vol_df$date[e],
             y0 = 0, y1 = 1,
             fillcolor = "rgba(255,100,100,0.08)",
             line = list(width = 0))
      })

      vol_cols <- grep("^vol_", names(vol_df), value = TRUE)

      p <- plotly::plot_ly()
      for (vc in vol_cols) {
        cn_label <- sub(paste0(".*_C"), "C", vc)
        p <- plotly::add_lines(p,
          data = vol_df, x = ~date, y = stats::as.formula(paste0("~`", vc, "`")),
          name = cn_label,
          hovertemplate = paste0("%{x|%Y-%m-%d} ", cn_label, ": %{y:.1%}<extra></extra>")
        )
      }

      p |>
        plotly::layout(
          shapes    = shapes,
          xaxis     = list(title = ""),
          yaxis     = list(title = "Annualised Vol", tickformat = ".0%"),
          legend    = list(orientation = "h"),
          hovermode = "x unified"
        )
    }) |> bindEvent(wide(), sel$window(), input$contracts)

    # ── Vol Cone ──────────────────────────────────────────────────────────────
    output$vol_cone <- plotly::renderPlotly({
      cone <- build_vol_cone(wide(), prefix(), contract_n = 1L)
      req(cone)

      current_vols <- vol_term_structure_snapshot(
        wide(), prefix(), max(wide()$date, na.rm = TRUE),
        window = sel$window()
      )
      c1_current <- if (!is.null(current_vols) && nrow(current_vols) > 0L)
        current_vols$vol[1L] else NA_real_

      p <- plotly::plot_ly(cone, x = ~horizon) |>
        plotly::add_ribbons(
          ymin = ~p25, ymax = ~p75,
          name = "25th–75th pct",
          fillcolor = "rgba(31,119,180,0.2)",
          line = list(width = 0)
        ) |>
        plotly::add_ribbons(
          ymin = ~p10, ymax = ~p90,
          name = "10th–90th pct",
          fillcolor = "rgba(31,119,180,0.08)",
          line = list(width = 0)
        ) |>
        plotly::add_lines(
          y = ~p50, name = "Median",
          line = list(color = "#1f77b4", width = 2)
        )

      if (!is.na(c1_current)) {
        p <- plotly::add_markers(p,
          x = sel$window(), y = c1_current,
          name = "Current",
          marker = list(color = "red", size = 10, symbol = "diamond")
        )
      }

      p |>
        plotly::layout(
          xaxis = list(title = "Lookback window (trading days)"),
          yaxis = list(title = "Annualised C1 Realised Vol", tickformat = ".0%"),
          legend = list(orientation = "h")
        )
    }) |> bindEvent(wide(), sel$window())
  })
}
