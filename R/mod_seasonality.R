# mod_seasonality.R
# Seasonality analysis: monthly return distributions, seasonal index, STL, YoY.

mod_seasonality_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel")),
      hr(),
      numericInput(ns("contract_n"), "Contract #", value = 1L, min = 1L, max = 36L)
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Monthly Returns",
        plotly::plotlyOutput(ns("monthly_box"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Seasonal Index",
        plotly::plotlyOutput(ns("seasonal_idx"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Year-over-Year",
        plotly::plotlyOutput(ns("yoy"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "STL Decomposition",
        plotly::plotlyOutput(ns("stl"), height = "70%", width = "100%")
      )
    )
  )
}

mod_seasonality_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    # Full history (no date filter) per selected market — required for seasonality
    wides_full <- reactive({
      mkts <- sel$market()
      req(length(mkts) > 0L)
      setNames(lapply(mkts, function(m) {
        req(mkt_data()[[m]])
        mkt_data()[[m]]$wide
      }), mkts)
    }) %>% bindEvent(sel$market())

    # Cap contract_n to min max_contracts across selected markets
    observeEvent(sel$market(), {
      mkts <- sel$market()
      if (length(mkts) > 0L) {
        max_c <- min(sapply(mkts, function(m) MARKETS[[m]]$max_contracts))
        updateNumericInput(session, "contract_n",
                           max   = max_c,
                           value = min(input$contract_n, max_c))
      }
    }, ignoreInit = TRUE)

    # ── Monthly return box plots ──────────────────────────────────────────────
    # Grouped box plots: one colour group per market, boxes aligned by month.
    output$monthly_box <- plotly::renderPlotly({
      w_list <- wides_full()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        mr  <- compute_monthly_returns(w_list[[m]], MARKETS[[m]]$rtl_prefix, input$contract_n)
        if (is.null(mr)) next
        lbl <- MARKETS[[m]]$label
        col <- MARKETS[[m]]$color

        p <- plotly::add_trace(p,
          data          = mr, x = ~month, y = ~monthly_ret,
          type          = "box",
          name          = lbl,
          boxpoints     = "outliers",
          marker        = list(color = col, size = 4),
          line          = list(color = col),
          fillcolor     = paste0(col, "44"),
          hovertemplate = paste0(lbl, " %{x}: %{y:.2%}<extra></extra>")
        )
      }

      p %>% plotly::layout(
        boxmode = "group",
        xaxis   = list(title = "Month", categoryorder = "array",
                       categoryarray = month.abb),
        yaxis   = list(title = "Monthly log return", tickformat = ".1%"),
        legend  = list(orientation = "h"),
        shapes  = list(list(
          type = "line", x0 = -0.5, x1 = 11.5, y0 = 0, y1 = 0,
          line = list(color = "#adb5bd", dash = "dot", width = 1)
        ))
      )
    }) %>% bindEvent(wides_full(), input$contract_n)

    # ── Seasonal index ────────────────────────────────────────────────────────
    # IQR ribbon + mean index line per market, overlaid on shared axes.
    output$seasonal_idx <- plotly::renderPlotly({
      w_list <- wides_full()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        si  <- seasonal_index(w_list[[m]], MARKETS[[m]]$rtl_prefix, input$contract_n)
        if (is.null(si)) next
        lbl <- MARKETS[[m]]$label
        col <- MARKETS[[m]]$color

        p <- p %>%
          plotly::add_ribbons(
            data        = si, x = ~month_num, ymin = ~lower, ymax = ~upper,
            name        = paste0(lbl, " IQR (25th\u201375th)"),
            legendgroup = lbl,
            fillcolor   = paste0(col, "33"),
            line        = list(width = 0)
          ) %>%
          plotly::add_lines(
            data          = si, x = ~month_num, y = ~seasonal_index,
            name          = paste0(lbl, " Seasonal Index"),
            legendgroup   = lbl,
            line          = list(color = col, width = 2.5),
            hovertemplate = paste0(lbl, " %{x}: %{y:.3f}<extra></extra>")
          )
      }

      # Reference line at 1.0 (seasonal average)
      p %>%
        plotly::add_lines(
          x = 1:12, y = rep(1, 12),
          line = list(color = "#adb5bd", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        plotly::layout(
          xaxis  = list(title = "Month", ticktext = month.abb, tickvals = 1:12),
          yaxis  = list(title = "Seasonal index (1.00 = average)"),
          legend = list(orientation = "h")
        )
    }) %>% bindEvent(wides_full(), input$contract_n)

    # ── Year-over-year overlay ────────────────────────────────────────────────
    # Each market gets its own colour ramp (grey → market colour).
    # Legend entries are grouped per market so toggling one market hides all
    # its year traces simultaneously.
    output$yoy <- plotly::renderPlotly({
      w_list <- wides_full()
      mkts   <- names(w_list)

      p <- plotly::plot_ly()
      for (m in mkts) {
        yoy_df <- yoy_normalized(w_list[[m]], MARKETS[[m]]$rtl_prefix, input$contract_n)
        if (is.null(yoy_df)) next
        lbl <- MARKETS[[m]]$label
        col <- MARKETS[[m]]$color

        years  <- sort(unique(yoy_df$year))
        n_yrs  <- length(years)
        colors <- colorRampPalette(c("#d0d0d0", col))(n_yrs)

        for (i in seq_along(years)) {
          yr_df <- dplyr::filter(yoy_df, year == years[i])
          p <- plotly::add_lines(p,
            data          = yr_df, x = ~day_of_year, y = ~normalized,
            name          = paste0(lbl, " ", years[i]),
            legendgroup   = lbl,
            line          = list(color = colors[i],
                                 width = if (i == n_yrs) 2.5 else 1),
            hovertemplate = paste0(lbl, " ", years[i], " Day %{x}: %{y:.1f}<extra></extra>")
          )
        }
      }

      p %>% plotly::layout(
        xaxis  = list(title = "Day of year",
                      ticktext = c("Jan","Feb","Mar","Apr","May","Jun",
                                   "Jul","Aug","Sep","Oct","Nov","Dec"),
                      tickvals = c(1,32,60,91,121,152,182,213,244,274,305,335)),
        yaxis  = list(title = "Normalised price (Jan 1 = 100)"),
        legend = list(orientation = "h")
      )
    }) %>% bindEvent(wides_full(), input$contract_n)

    # ── STL decomposition ─────────────────────────────────────────────────────
    # Layout is component-first: all markets' Trend panels, then all Seasonal,
    # then all Residual — enabling direct cross-market comparison per component.
    #
    #   Row 1:   Trend — Market 1
    #   Row 2:   Trend — Market 2
    #   Row 3:   Seasonal — Market 1
    #   Row 4:   Seasonal — Market 2
    #   Row 5:   Residual — Market 1
    #   Row 6:   Residual — Market 2
    output$stl <- plotly::renderPlotly({
      w_list <- wides_full()
      mkts   <- names(w_list)

      stl_list <- lapply(setNames(mkts, mkts), function(m) {
        stl_decompose(w_list[[m]], MARKETS[[m]]$rtl_prefix, input$contract_n)
      })
      stl_list <- Filter(Negate(is.null), stl_list)
      req(length(stl_list) > 0L)

      n         <- length(stl_list)
      mkt_names <- names(stl_list)

      # Build 3*N subplot panels in component-first order:
      # indices 1..n   → Trend panels
      # indices n+1..2n → Seasonal panels
      # indices 2n+1..3n → Residual panels
      all_plots <- vector("list", 3L * n)
      for (i in seq_len(n)) {
        m   <- mkt_names[i]
        lbl <- MARKETS[[m]]$label
        col <- MARKETS[[m]]$color
        df  <- stl_list[[m]]

        all_plots[[i]] <- plotly::plot_ly(df, x = ~date, y = ~trend,
          type = "scatter", mode = "lines",
          name = paste0(lbl, " Trend"),
          line = list(color = col, width = 1.5)
        ) %>% plotly::layout(yaxis = list(title = paste0(lbl, " — Trend")))

        all_plots[[n + i]] <- plotly::plot_ly(df, x = ~date, y = ~seasonal,
          type = "scatter", mode = "lines",
          name = paste0(lbl, " Seasonal"),
          line = list(color = col, width = 1.5)
        ) %>% plotly::layout(yaxis = list(title = paste0(lbl, " — Seasonal")))

        all_plots[[2L * n + i]] <- plotly::plot_ly(df, x = ~date, y = ~remainder,
          type   = "bar",
          name   = paste0(lbl, " Residual"),
          marker = list(color = paste0(col, "aa"))
        ) %>% plotly::layout(yaxis = list(title = paste0(lbl, " — Residual")))
      }

      plotly::subplot(all_plots, nrows = 3L * n, shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          xaxis      = list(title = ""),
          legend     = list(orientation = "h"),
          showlegend = TRUE
        )
    }) %>% bindEvent(wides_full(), input$contract_n)
  })
}
