# mod_correlation.R
# Cross-market rolling correlations, spreads, and spread z-scores.

mod_correlation_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      dateRangeInput(
        ns("date_range"),
        label = "Date range",
        start = Sys.Date() - 365 * 3,
        end   = Sys.Date()
      ),
      actionButton(
        ns("full_history"), "Full history",
        class = "btn btn-sm btn-outline-secondary w-100 mb-2"
      ),
      radioButtons(
        ns("window"),
        label    = "Rolling window",
        choices  = c("21d" = 21L, "63d" = 63L, "126d" = 126L, "252d" = 252L),
        selected = DEFAULT_WINDOW,
        inline   = TRUE
      ),
      hr(),
      selectInput(
        ns("spread_x"),
        label   = "Spread: Market A",
        choices = stats::setNames(
          ENABLED_MARKETS,
          sapply(ENABLED_MARKETS, function(m) MARKETS[[m]]$label)
        ),
        selected = ENABLED_MARKETS[1L]
      ),
      selectInput(
        ns("spread_y"),
        label   = "Spread: Market B",
        choices = stats::setNames(
          ENABLED_MARKETS,
          sapply(ENABLED_MARKETS, function(m) MARKETS[[m]]$label)
        ),
        selected = if (length(ENABLED_MARKETS) >= 2L) ENABLED_MARKETS[2L] else ENABLED_MARKETS[1L]
      )
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Correlation Heatmap",
        plotly::plotlyOutput(ns("corr_heatmap"), height = "480px")
      ),
      bslib::nav_panel(
        "Rolling Correlation",
        plotly::plotlyOutput(ns("rolling_corr"), height = "420px")
      ),
      bslib::nav_panel(
        "Spread",
        plotly::plotlyOutput(ns("spread_plot"), height = "420px")
      ),
      bslib::nav_panel(
        "Spread Z-Score",
        plotly::plotlyOutput(ns("spread_zscore"), height = "420px")
      )
    )
  )
}

mod_correlation_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
                           start = as.Date("2007-01-01"),
                           end   = Sys.Date())
    })

    window <- reactive({
      w <- input$window
      if (is.null(w)) DEFAULT_WINDOW else as.integer(w)
    })

    # Build a wide data.frame of C1 log-returns for all enabled markets
    ret_wide <- reactive({
      all_data <- mkt_data()
      req(all_data)
      dfs <- purrr::imap(all_data, function(mkt_obj, mkt_key) {
        rw <- mkt_obj$ret_wide
        req(rw)
        pfx    <- MARKETS[[mkt_key]]$rtl_prefix
        c1_col <- paste0("ret_", pfx, "_C1")
        if (!c1_col %in% names(rw)) return(NULL)
        df <- rw[, c("date", c1_col)]
        names(df)[2] <- mkt_key
        df
      })
      dfs <- Filter(Negate(is.null), dfs)
      if (length(dfs) == 0L) return(NULL)
      Reduce(function(a, b) dplyr::full_join(a, b, by = "date"), dfs) |>
        dplyr::arrange(date)
    })

    # Filtered by date range
    ret_filtered <- reactive({
      df <- ret_wide()
      req(df)
      date_from <- input$date_range[1]
      date_to   <- input$date_range[2]
      filter_date_range(df, date_from, date_to)
    }) |> bindEvent(ret_wide(), input$date_range)

    # ── Correlation Heatmap (full-period) ─────────────────────────────────────
    output$corr_heatmap <- plotly::renderPlotly({
      df <- ret_filtered()
      req(df, nrow(df) > 10L)

      num_cols <- intersect(ENABLED_MARKETS, names(df))
      req(length(num_cols) >= 2L)

      mat   <- as.matrix(df[, num_cols])
      corr  <- cor(mat, use = "pairwise.complete.obs")
      lbls  <- sapply(num_cols, function(m) MARKETS[[m]]$label)
      rownames(corr) <- lbls
      colnames(corr) <- lbls

      plotly::plot_ly(
        x          = colnames(corr),
        y          = rownames(corr),
        z          = corr,
        type       = "heatmap",
        colorscale = list(
          c(0,   "#c0392b"),
          c(0.5, "#ffffff"),
          c(1,   "#1a7340")
        ),
        zmin = -1, zmax = 1,
        text = round(corr, 2),
        hovertemplate = "%{y} / %{x}: %{z:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "")
        )
    }) |> bindEvent(ret_filtered())

    # ── Rolling Pairwise Correlation (A vs B) ─────────────────────────────────
    output$rolling_corr <- plotly::renderPlotly({
      df  <- ret_filtered()
      req(df)
      mx  <- input$spread_x
      my  <- input$spread_y
      req(mx %in% names(df), my %in% names(df), mx != my)

      win <- window()
      rc  <- slider::slide_dbl(
        seq_len(nrow(df)),
        function(idx) {
          x_vals <- df[[mx]][idx]
          y_vals <- df[[my]][idx]
          if (sum(!is.na(x_vals) & !is.na(y_vals)) < 5L) return(NA_real_)
          cor(x_vals, y_vals, use = "complete.obs")
        },
        .before = win - 1L,
        .complete = TRUE
      )

      plot_df <- data.frame(date = df$date, corr = rc)

      plotly::plot_ly(plot_df, x = ~date, y = ~corr,
        type = "scatter", mode = "lines",
        line = list(color = "#1f77b4", width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.3f}<extra></extra>"
      ) |>
        plotly::add_lines(
          y = ~0, line = list(color = "#adb5bd", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = glue::glue("{win}d Rolling Correlation"),
                       range = c(-1, 1)),
          title = list(text = glue::glue(
            "{MARKETS[[mx]]$label} vs {MARKETS[[my]]$label}"
          ))
        )
    }) |> bindEvent(ret_filtered(), input$spread_x, input$spread_y, window())

    # ── Spread (C1 price difference) ─────────────────────────────────────────
    spread_prices <- reactive({
      mx <- input$spread_x
      my <- input$spread_y
      req(mx %in% ENABLED_MARKETS, my %in% ENABLED_MARKETS)
      all_data <- mkt_data()
      req(all_data[[mx]], all_data[[my]])

      pfx_x  <- MARKETS[[mx]]$rtl_prefix
      pfx_y  <- MARKETS[[my]]$rtl_prefix
      col_x  <- paste0(pfx_x, "01")
      col_y  <- paste0(pfx_y, "01")

      wx <- all_data[[mx]]$wide
      wy <- all_data[[my]]$wide

      req(col_x %in% names(wx), col_y %in% names(wy))

      bbl_x <- MARKETS[[mx]]$bbl_factor
      bbl_y <- MARKETS[[my]]$bbl_factor

      dx <- wx[, c("date", col_x)]
      dy <- wy[, c("date", col_y)]
      names(dx)[2] <- "px"
      names(dy)[2] <- "py"

      df <- dplyr::inner_join(dx, dy, by = "date") |>
        dplyr::mutate(
          px_bbl = px * bbl_x,
          py_bbl = py * bbl_y,
          spread = px_bbl - py_bbl
        ) |>
        dplyr::arrange(date)

      filter_date_range(df, input$date_range[1], input$date_range[2])
    }) |> bindEvent(mkt_data(), input$spread_x, input$spread_y, input$date_range)

    output$spread_plot <- plotly::renderPlotly({
      df <- spread_prices()
      req(df, nrow(df) > 0L)

      mx <- input$spread_x
      my <- input$spread_y

      plotly::plot_ly(df, x = ~date, y = ~spread,
        type = "scatter", mode = "lines",
        line = list(color = "#1f77b4", width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}<extra></extra>"
      ) |>
        plotly::add_lines(
          y = ~0, line = list(color = "#adb5bd", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Spread ($/bbl equivalent)"),
          title = list(text = glue::glue(
            "{MARKETS[[mx]]$label} minus {MARKETS[[my]]$label} (C1, $/bbl)"
          ))
        )
    })

    # ── Spread Z-Score ────────────────────────────────────────────────────────
    output$spread_zscore <- plotly::renderPlotly({
      df  <- spread_prices()
      req(df, nrow(df) > 0L)

      win <- window()
      roll_mean <- slider::slide_dbl(df$spread, mean, na.rm = TRUE,
                                     .before = win - 1L, .complete = FALSE)
      roll_sd   <- slider::slide_dbl(df$spread, sd,   na.rm = TRUE,
                                     .before = win - 1L, .complete = FALSE)

      df$zscore <- (df$spread - roll_mean) / roll_sd

      mx <- input$spread_x
      my <- input$spread_y

      plotly::plot_ly(df, x = ~date, y = ~zscore,
        type = "scatter", mode = "lines",
        line = list(color = "#ff7f0e", width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}σ<extra></extra>"
      ) |>
        plotly::add_lines(
          y = ~0,   line = list(color = "#adb5bd", dash = "dot",  width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::add_lines(
          y = ~ 2,  line = list(color = "#c0392b", dash = "dash", width = 1),
          name = "+2σ"
        ) |>
        plotly::add_lines(
          y = ~-2,  line = list(color = "#1a7340", dash = "dash", width = 1),
          name = "-2σ"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = glue::glue("{win}d Rolling Z-Score")),
          title = list(text = glue::glue(
            "Spread Z-Score: {MARKETS[[mx]]$label} − {MARKETS[[my]]$label}"
          )),
          legend = list(orientation = "h")
        )
    }) |> bindEvent(spread_prices(), window())
  })
}
