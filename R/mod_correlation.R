# mod_correlation.R
# Cross-market co-dynamics: rolling correlations, spreads, CCF, spread term structure.

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
        label   = "Market A",
        choices = stats::setNames(
          ENABLED_MARKETS,
          sapply(ENABLED_MARKETS, function(m) MARKETS[[m]]$label)
        ),
        selected = ENABLED_MARKETS[1L]
      ),
      selectInput(
        ns("spread_y"),
        label   = "Market B",
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
      ),
      bslib::nav_panel(
        "Spread Term Structure",
        plotly::plotlyOutput(ns("spread_ts"), height = "420px")
      ),
      bslib::nav_panel(
        "CCF (Lead-Lag)",
        plotly::plotlyOutput(ns("ccf_plot"), height = "420px")
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

    # ── Wide data.frame of C1 log-returns for all enabled markets ─────────────
    ret_wide <- reactive({
      all_data <- mkt_data()
      req(all_data)
      dfs <- purrr::imap(all_data, function(mkt_obj, mkt_key) {
        rw     <- mkt_obj$ret_wide
        req(rw)
        pfx    <- MARKETS[[mkt_key]]$rtl_prefix
        c1_col <- paste0("ret_", pfx, "_C1")
        if (!c1_col %in% names(rw)) return(NULL)
        df <- rw[, c("date", c1_col)]
        names(df)[2L] <- mkt_key
        df
      })
      dfs <- Filter(Negate(is.null), dfs)
      if (length(dfs) == 0L) return(NULL)
      Reduce(function(a, b) dplyr::full_join(a, b, by = "date"), dfs) |>
        dplyr::arrange(date)
    })

    # Filtered to date range
    ret_filtered <- reactive({
      df        <- ret_wide()
      req(df)
      date_from <- input$date_range[1]
      date_to   <- input$date_range[2]
      filter_date_range(df, date_from, date_to)
    }) |> bindEvent(ret_wide(), input$date_range)

    # ── Correlation Heatmap (full-period correlation matrix) ──────────────────
    output$corr_heatmap <- plotly::renderPlotly({
      df <- ret_filtered()
      req(df, nrow(df) > 10L)

      num_cols <- intersect(ENABLED_MARKETS, names(df))
      req(length(num_cols) >= 2L)

      corr <- cor(as.matrix(df[, num_cols]), use = "pairwise.complete.obs")
      lbls <- sapply(num_cols, function(m) MARKETS[[m]]$label)
      rownames(corr) <- lbls
      colnames(corr) <- lbls

      plotly::plot_ly(
        x          = colnames(corr),
        y          = rownames(corr),
        z          = corr,
        type       = "heatmap",
        colorscale = list(c(0, "#c0392b"), c(0.5, "#ffffff"), c(1, "#1a7340")),
        zmin = -1, zmax = 1,
        text = round(corr, 2L),
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

      # slide2_dbl slides two parallel vectors simultaneously
      rc <- slider::slide2_dbl(
        df[[mx]], df[[my]],
        .f = ~ {
          valid <- !is.na(.x) & !is.na(.y)
          if (sum(valid) < 5L) return(NA_real_)
          cor(.x[valid], .y[valid])
        },
        .before   = win - 1L,
        .complete = TRUE
      )

      plot_df <- data.frame(date = df$date, corr = rc)

      plotly::plot_ly(plot_df, x = ~date, y = ~corr,
        type = "scatter", mode = "lines",
        line = list(color = "#1f77b4", width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis  = list(title = ""),
          yaxis  = list(title = glue::glue("{win}d Rolling Correlation"), range = c(-1, 1)),
          title  = list(text = glue::glue(
            "{MARKETS[[mx]]$label} vs {MARKETS[[my]]$label}"
          )),
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1,
            y0 = 0, y1 = 0,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )
    }) |> bindEvent(ret_filtered(), input$spread_x, input$spread_y, window())

    # ── Spread (C1 price difference, $/bbl equivalent) ────────────────────────
    spread_prices <- reactive({
      mx <- input$spread_x
      my <- input$spread_y
      req(mx %in% ENABLED_MARKETS, my %in% ENABLED_MARKETS)
      all_data <- mkt_data()
      req(all_data[[mx]], all_data[[my]])

      pfx_x <- MARKETS[[mx]]$rtl_prefix
      pfx_y <- MARKETS[[my]]$rtl_prefix
      col_x <- paste0(pfx_x, "_C1")
      col_y <- paste0(pfx_y, "_C1")

      wx <- all_data[[mx]]$wide
      wy <- all_data[[my]]$wide

      req(col_x %in% names(wx), col_y %in% names(wy))

      bbl_x <- MARKETS[[mx]]$bbl_factor
      bbl_y <- MARKETS[[my]]$bbl_factor

      dx <- wx[, c("date", col_x)]
      dy <- wy[, c("date", col_y)]
      names(dx)[2L] <- "px"
      names(dy)[2L] <- "py"

      dplyr::inner_join(dx, dy, by = "date") |>
        dplyr::mutate(
          px_bbl = px * bbl_x,
          py_bbl = py * bbl_y,
          spread = px_bbl - py_bbl
        ) |>
        dplyr::arrange(date) |>
        filter_date_range(input$date_range[1], input$date_range[2])
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
        plotly::layout(
          xaxis  = list(title = ""),
          yaxis  = list(title = "Spread ($/bbl equivalent)"),
          title  = list(text = glue::glue(
            "{MARKETS[[mx]]$label} \u2212 {MARKETS[[my]]$label} (C1, $/bbl)"
          )),
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1,
            y0 = 0, y1 = 0,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )
    }) |> bindEvent(spread_prices())

    # ── Spread Z-Score ────────────────────────────────────────────────────────
    output$spread_zscore <- plotly::renderPlotly({
      df  <- spread_prices()
      req(df, nrow(df) > 0L)

      win       <- window()
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
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.2f}\u03c3<extra></extra>"
      ) |>
        plotly::layout(
          xaxis  = list(title = ""),
          yaxis  = list(title = glue::glue("{win}d Rolling Z-Score")),
          title  = list(text = glue::glue(
            "Spread Z-Score: {MARKETS[[mx]]$label} \u2212 {MARKETS[[my]]$label}"
          )),
          legend = list(orientation = "h"),
          shapes = list(
            list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                 y0 =  2, y1 =  2,
                 line = list(color = "#c0392b", dash = "dash", width = 1)),
            list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                 y0 =  0, y1 =  0,
                 line = list(color = "#adb5bd", dash = "dot",  width = 1)),
            list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                 y0 = -2, y1 = -2,
                 line = list(color = "#1a7340", dash = "dash", width = 1))
          )
        )
    }) |> bindEvent(spread_prices(), window())

    # ── Spread Term Structure ─────────────────────────────────────────────────
    # Spread at each contract level (C1–C12) at the most recent available date.
    # Shows whether the spread widens or compresses along the forward curve.
    output$spread_ts <- plotly::renderPlotly({
      mx <- input$spread_x
      my <- input$spread_y
      req(mx %in% ENABLED_MARKETS, my %in% ENABLED_MARKETS, mx != my)

      all_data <- mkt_data()
      req(all_data[[mx]], all_data[[my]])

      pfx_x <- MARKETS[[mx]]$rtl_prefix
      pfx_y <- MARKETS[[my]]$rtl_prefix
      bbl_x <- MARKETS[[mx]]$bbl_factor
      bbl_y <- MARKETS[[my]]$bbl_factor

      wx <- all_data[[mx]]$wide
      wy <- all_data[[my]]$wide

      snap_d <- max(intersect(wx$date, wy$date))
      row_x  <- wx[wx$date == snap_d, , drop = FALSE]
      row_y  <- wy[wy$date == snap_d, , drop = FALSE]
      req(nrow(row_x) > 0L, nrow(row_y) > 0L)

      max_c <- min(MARKETS[[mx]]$max_contracts, MARKETS[[my]]$max_contracts, 12L)

      ts_df <- purrr::map_dfr(seq_len(max_c), function(n) {
        cx <- paste0(pfx_x, "_C", n)
        cy <- paste0(pfx_y, "_C", n)
        if (!cx %in% names(row_x) || !cy %in% names(row_y)) return(NULL)
        px <- as.numeric(row_x[[cx]]) * bbl_x
        py <- as.numeric(row_y[[cy]]) * bbl_y
        if (is.na(px) || is.na(py)) return(NULL)
        data.frame(contract = n, spread = px - py)
      })
      req(!is.null(ts_df), nrow(ts_df) >= 2L)

      plotly::plot_ly(ts_df, x = ~contract, y = ~spread,
        type = "scatter", mode = "lines+markers",
        line   = list(color = "#1f77b4", width = 2),
        marker = list(color = "#1f77b4", size = 8),
        hovertemplate = "C%{x}: %{y:.2f}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis  = list(title = "Contract", dtick = 1),
          yaxis  = list(title = "Spread ($/bbl equivalent)"),
          title  = list(text = glue::glue(
            "{MARKETS[[mx]]$label} \u2212 {MARKETS[[my]]$label} \u2014 {format(snap_d, '%b %d, %Y')} ($/bbl)"
          )),
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1,
            y0 = 0, y1 = 0,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )
    }) |> bindEvent(mkt_data(), input$spread_x, input$spread_y)

    # ── CCF (Lead-Lag) ────────────────────────────────────────────────────────
    # Cross-correlation function between front-month log returns for Markets A and B.
    # Positive lag k means A at time t correlates with B at time t+k (A leads B).
    # Blue bars exceed the 95% significance bound (±2/√n); grey bars do not.
    output$ccf_plot <- plotly::renderPlotly({
      df <- ret_filtered()
      mx <- input$spread_x
      my <- input$spread_y
      req(df, mx %in% names(df), my %in% names(df), mx != my)

      x     <- df[[mx]]
      y     <- df[[my]]
      valid <- !is.na(x) & !is.na(y)
      req(sum(valid) > 40L)

      cc     <- stats::ccf(x[valid], y[valid], lag.max = 20L, plot = FALSE)
      ccf_df <- data.frame(lag = as.integer(cc$lag), acf = as.numeric(cc$acf))

      # 95% significance bound for white-noise test
      sig_bnd    <- 2 / sqrt(sum(valid))
      bar_colors <- ifelse(abs(ccf_df$acf) > sig_bnd, "#1f77b4", "#adb5bd")

      plotly::plot_ly(ccf_df, x = ~lag, y = ~acf,
        type   = "bar",
        marker = list(color = bar_colors),
        hovertemplate = "Lag %{x}d: %{y:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis  = list(title = glue::glue(
            "Lag (days) \u2014 positive: {MARKETS[[mx]]$label} leads {MARKETS[[my]]$label}"
          ), dtick = 5),
          yaxis  = list(title = "Cross-correlation", range = c(-1, 1)),
          title  = list(text = glue::glue(
            "CCF: {MARKETS[[mx]]$label} (x, t) vs {MARKETS[[my]]$label} (y, t+lag)"
          )),
          shapes = list(
            list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                 y0 =  sig_bnd, y1 =  sig_bnd,
                 line = list(color = "#c0392b", dash = "dash", width = 1)),
            list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                 y0 = -sig_bnd, y1 = -sig_bnd,
                 line = list(color = "#c0392b", dash = "dash", width = 1))
          )
        )
    }) |> bindEvent(ret_filtered(), input$spread_x, input$spread_y)
  })
}
