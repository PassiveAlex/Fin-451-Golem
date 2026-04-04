# mod_volatility.R
# Volatility: realized vol term structure, rolling vol over time, vol cone.

mod_volatility_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel"))
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Vol Term Structure",
        plotly::plotlyOutput(ns("vol_ts"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Vol Over Time",
        plotly::plotlyOutput(ns("vol_history"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Vol Cone",
        plotly::plotlyOutput(ns("vol_cone"), height = "70%", width = "100%")
      )
    )
  )
}

mod_volatility_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    # Named list of filtered wide data, one entry per selected market
    wides <- reactive({
      mkts <- sel$market()
      req(length(mkts) > 0L)
      setNames(lapply(mkts, function(m) {
        req(mkt_data()[[m]])
        filter_date_range(mkt_data()[[m]]$wide, sel$date_from(), sel$date_to())
      }), mkts)
    }) %>% bindEvent(sel$market(), sel$date_from(), sel$date_to())

    # ── Vol Term Structure snapshot ───────────────────────────────────────────
    # Grouped bars: one bar per (contract, market) pair at each contract position.
    output$vol_ts <- plotly::renderPlotly({
      w_list <- wides()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        w         <- w_list[[m]]
        pfx       <- MARKETS[[m]]$rtl_prefix
        lbl       <- MARKETS[[m]]$label
        snap_date <- max(w$date, na.rm = TRUE)
        vts       <- vol_term_structure_snapshot(w, pfx, snap_date, window = sel$window())
        if (is.null(vts)) next

        p <- plotly::add_bars(p,
          data          = vts, x = ~contract, y = ~vol,
          name          = lbl,
          marker        = list(color = MARKETS[[m]]$color),
          hovertemplate = paste0(lbl, " C%{x}: %{y:.1%}<extra></extra>")
        )
      }

      p %>% plotly::layout(
        barmode = "group",
        xaxis   = list(title = "Contract", dtick = 3),
        yaxis   = list(title = glue::glue("{sel$window()}d Ann. Realised Vol"),
                       tickformat = ".0%"),
        legend  = list(orientation = "h")
      )
    }) %>% bindEvent(wides(), sel$window())

    # ── Vol Over Time ─────────────────────────────────────────────────────────
    # One set of contract lines per market. C1/C3/C6/C12 visible by default,
    # all others as legendonly. High-vol regime shading uses the first selected
    # market's C1 series as the reference.
    output$vol_history <- plotly::renderPlotly({
      w_list <- wides()
      mkts   <- names(w_list)

      # Compute regime shading from first market's C1
      first_m   <- mkts[1L]
      first_pfx <- MARKETS[[first_m]]$rtl_prefix
      first_max <- MARKETS[[first_m]]$max_contracts
      first_vol <- rolling_vol_wide(w_list[[first_m]], first_pfx,
                                    seq_len(first_max), window = sel$window())
      c1_col      <- paste0("vol_", first_pfx, "_C1")
      regime_flag <- if (c1_col %in% names(first_vol)) {
        vol_regime_high(first_vol[[c1_col]])
      } else rep(FALSE, nrow(first_vol))

      run_start <- which(diff(c(FALSE, regime_flag)) == 1L)
      run_end   <- which(diff(c(regime_flag, FALSE)) == -1L)
      shapes <- purrr::map2(run_start, run_end, function(s, e) {
        list(type = "rect", xref = "x", yref = "paper",
             x0 = first_vol$date[s], x1 = first_vol$date[e],
             y0 = 0, y1 = 1,
             fillcolor = "rgba(255,100,100,0.08)",
             line = list(width = 0))
      })

      p <- plotly::plot_ly()
      for (m in mkts) {
        w        <- w_list[[m]]
        pfx      <- MARKETS[[m]]$rtl_prefix
        lbl      <- MARKETS[[m]]$label
        max_c    <- MARKETS[[m]]$max_contracts
        all_cs   <- seq_len(max_c)
        vol_df   <- rolling_vol_wide(w, pfx, all_cs, window = sel$window())
        vol_cols <- grep(paste0("^vol_", pfx, "_C\\d+$"), names(vol_df), value = TRUE)
        default_shown <- intersect(c(1L, 3L, 6L, 12L), all_cs)

        for (vc in vol_cols) {
          cn_num   <- as.integer(sub(paste0("^vol_", pfx, "_C"), "", vc))
          cn_label <- paste0(lbl, "-C", cn_num)
          p <- plotly::add_lines(p,
            data          = vol_df,
            x             = ~date,
            y             = stats::as.formula(paste0("~`", vc, "`")),
            name          = cn_label,
            line          = list(color = MARKETS[[m]]$color),
            visible       = if (cn_num %in% default_shown) TRUE else "legendonly",
            hovertemplate = paste0("%{x|%Y-%m-%d} ", cn_label, ": %{y:.1%}<extra></extra>")
          )
        }
      }

      p %>% plotly::layout(
        shapes    = shapes,
        xaxis     = list(title = ""),
        yaxis     = list(title = glue::glue("{sel$window()}d Ann. Realised Vol"),
                         tickformat = ".0%"),
        legend    = list(orientation = "h"),
        hovermode = "x unified"
      )
    }) %>% bindEvent(wides(), sel$window())

    # ── Vol Cone ──────────────────────────────────────────────────────────────
    # One set of percentile ribbons per market. Current vol marker at sel$window().
    output$vol_cone <- plotly::renderPlotly({
      w_list <- wides()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        w         <- w_list[[m]]
        pfx       <- MARKETS[[m]]$rtl_prefix
        lbl       <- MARKETS[[m]]$label
        mkt_color <- MARKETS[[m]]$color
        cone      <- build_vol_cone(w, pfx, contract_n = 1L)
        if (is.null(cone)) next

        hex   <- sub("^#", "", mkt_color)
        r_val <- strtoi(substr(hex, 1L, 2L), 16L)
        g_val <- strtoi(substr(hex, 3L, 4L), 16L)
        b_val <- strtoi(substr(hex, 5L, 6L), 16L)

        p <- p %>%
          plotly::add_ribbons(
            data        = cone, x = ~horizon, ymin = ~p25, ymax = ~p75,
            name        = paste0(lbl, " 25th\u201375th"),
            legendgroup = lbl,
            fillcolor   = sprintf("rgba(%d,%d,%d,0.20)", r_val, g_val, b_val),
            line        = list(width = 0)
          ) %>%
          plotly::add_ribbons(
            data        = cone, x = ~horizon, ymin = ~p10, ymax = ~p90,
            name        = paste0(lbl, " 10th\u201390th"),
            legendgroup = lbl,
            fillcolor   = sprintf("rgba(%d,%d,%d,0.08)", r_val, g_val, b_val),
            line        = list(width = 0)
          ) %>%
          plotly::add_lines(
            data        = cone, x = ~horizon, y = ~p50,
            name        = paste0(lbl, " Median"),
            legendgroup = lbl,
            line        = list(color = mkt_color, width = 2)
          )

        current_vols <- vol_term_structure_snapshot(
          w, pfx, max(w$date, na.rm = TRUE), window = sel$window()
        )
        c1_current <- if (!is.null(current_vols) && nrow(current_vols) > 0L)
          current_vols$vol[1L] else NA_real_

        if (!is.na(c1_current)) {
          p <- plotly::add_markers(p,
            x           = sel$window(), y = c1_current,
            name        = paste0(lbl, " Current"),
            legendgroup = lbl,
            marker      = list(color = mkt_color, size = 10, symbol = "diamond")
          )
        }
      }

      p %>% plotly::layout(
        xaxis  = list(title = "Lookback window (trading days)"),
        yaxis  = list(title = "Ann. Realised Vol", tickformat = ".0%"),
        legend = list(orientation = "h")
      )
    }) %>% bindEvent(wides(), sel$window())
  })
}
