# mod_hedge_ratios.R
# Hedge ratio dynamics: rolling OLS beta, hedge effectiveness, term structure, basis risk.
# Supports two modes:
#   Per Market  — same exposure/hedge contract applied independently to each selected market
#   Cross Market — exposure and hedge instruments drawn from two different markets

mod_hedge_ratios_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel")),
      hr(),
      radioButtons(ns("hr_mode"), "Hedge mode",
        choices  = c("Per Market" = "per_market", "Cross Market" = "cross_market"),
        selected = "per_market",
        inline   = TRUE
      ),
      # ── Per-market contract selectors ────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("hr_mode"), "'] === 'per_market'"),
        numericInput(ns("exp_c"),   "Exposure contract", value = 1L, min = 1L, max = 36L),
        numericInput(ns("hedge_c"), "Hedge contract",    value = 3L, min = 2L, max = 36L),
        helpText("Term structure: OLS beta hedging the exposure contract",
                 "using C2 through C12 (full history).")
      ),
      # ── Cross-market selectors ───────────────────────────────────────────────
      conditionalPanel(
        condition = paste0("input['", ns("hr_mode"), "'] === 'cross_market'"),
        helpText("Date range and rolling window above still apply."),
        selectInput(ns("exp_mkt"), "Exposure market",
          choices  = stats::setNames(
            ENABLED_MARKETS,
            sapply(ENABLED_MARKETS, function(m) MARKETS[[m]]$label)
          ),
          selected = ENABLED_MARKETS[1L]
        ),
        numericInput(ns("exp_c_cross"),   "Exposure contract", value = 1L, min = 1L, max = 36L),
        selectInput(ns("hedge_mkt"), "Hedge market",
          choices  = stats::setNames(
            ENABLED_MARKETS,
            sapply(ENABLED_MARKETS, function(m) MARKETS[[m]]$label)
          ),
          selected = ENABLED_MARKETS[2L]
        ),
        numericInput(ns("hedge_c_cross"), "Hedge contract", value = 1L, min = 1L, max = 36L),
        helpText("Term structure: varies hedge contract (C1\u2013C12) for the fixed exposure.")
      )
    ),
    uiOutput(ns("value_boxes")),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Rolling Hedge Ratio",
        plotly::plotlyOutput(ns("rolling_hr"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Term Structure",
        plotly::plotlyOutput(ns("hr_ts"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Basis Risk",
        plotly::plotlyOutput(ns("basis_risk"), height = "70%", width = "100%")
      )
    )
  )
}

mod_hedge_ratios_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    # ── Per-market: cap exp_c / hedge_c to min max_contracts ─────────────────
    observeEvent(sel$market(), {
      mkts <- sel$market()
      if (length(mkts) == 0L) return()
      max_c <- min(sapply(mkts, function(m) MARKETS[[m]]$max_contracts))
      updateNumericInput(session, "exp_c",   max = max_c, value = min(input$exp_c,   max_c))
      updateNumericInput(session, "hedge_c", max = max_c, value = min(input$hedge_c, max_c))
    }, ignoreInit = TRUE)

    # ── Cross-market: cap per-market contract limits ──────────────────────────
    observeEvent(input$exp_mkt, {
      max_c <- MARKETS[[input$exp_mkt]]$max_contracts
      updateNumericInput(session, "exp_c_cross",
                         max = max_c, value = min(input$exp_c_cross, max_c))
    }, ignoreInit = TRUE)

    observeEvent(input$hedge_mkt, {
      max_c <- MARKETS[[input$hedge_mkt]]$max_contracts
      updateNumericInput(session, "hedge_c_cross",
                         max = max_c, value = min(input$hedge_c_cross, max_c))
    }, ignoreInit = TRUE)

    # ── Per-market: filtered wide data ───────────────────────────────────────
    wides_pm <- reactive({
      mkts <- sel$market()
      req(length(mkts) > 0L)
      setNames(lapply(mkts, function(m) {
        req(mkt_data()[[m]])
        filter_date_range(mkt_data()[[m]]$wide, sel$date_from(), sel$date_to())
      }), mkts)
    }) %>% bindEvent(sel$market(), sel$date_from(), sel$date_to())

    # ── Per-market: rolling HR series for each selected market ────────────────
    hr_series_list <- reactive({
      req(input$hr_mode == "per_market")
      w_list <- wides_pm()
      mkts   <- names(w_list)
      setNames(lapply(mkts, function(m) {
        rolling_hedge_ratio_series(w_list[[m]], MARKETS[[m]]$rtl_prefix,
                                   input$exp_c, input$hedge_c,
                                   window = sel$window())
      }), mkts)
    })

    # ── Cross-market: filtered wide for exposure and hedge markets ────────────
    exp_wide_cm <- reactive({
      req(input$hr_mode == "cross_market", input$exp_mkt)
      req(mkt_data()[[input$exp_mkt]])
      filter_date_range(mkt_data()[[input$exp_mkt]]$wide, sel$date_from(), sel$date_to())
    })

    hedge_wide_cm <- reactive({
      req(input$hr_mode == "cross_market", input$hedge_mkt)
      req(mkt_data()[[input$hedge_mkt]])
      filter_date_range(mkt_data()[[input$hedge_mkt]]$wide, sel$date_from(), sel$date_to())
    })

    # ── Cross-market: rolling HR series ──────────────────────────────────────
    hr_series_cm <- reactive({
      req(input$hr_mode == "cross_market")
      rolling_hedge_ratio_cross_market(
        exp_wide_cm(),   MARKETS[[input$exp_mkt]]$rtl_prefix,   input$exp_c_cross,
        hedge_wide_cm(), MARKETS[[input$hedge_mkt]]$rtl_prefix, input$hedge_c_cross,
        window = sel$window()
      )
    })

    # ── Value boxes ───────────────────────────────────────────────────────────
    output$value_boxes <- renderUI({
      if (isTRUE(input$hr_mode == "per_market")) {
        hrs  <- hr_series_list()
        mkts <- names(hrs)
        rows <- lapply(mkts, function(m) {
          lbl <- MARKETS[[m]]$label
          hr  <- hrs[[m]]
          beta_val <- if (!is.null(hr)) {
            b <- tail(hr$beta[!is.na(hr$beta)], 1L)
            if (length(b) == 0L) "N/A" else sprintf("%.3f", b)
          } else "N/A"
          r2_val <- if (!is.null(hr)) {
            r2 <- tail(hr$r_squared[!is.na(hr$r_squared)], 1L)
            if (length(r2) == 0L) "N/A" else scales::percent(r2, accuracy = 0.1)
          } else "N/A"

          bslib::layout_column_wrap(
            width = 1/2,
            bslib::value_box(
              title    = paste0(lbl, " \u2014 Hedge Ratio (beta)"),
              value    = beta_val,
              showcase = bsicons::bs_icon("sliders")
            ),
            bslib::value_box(
              title    = paste0(lbl, " \u2014 Hedge Effectiveness (R\u00b2)"),
              value    = r2_val,
              showcase = bsicons::bs_icon("check2-circle")
            )
          )
        })
        do.call(tagList, rows)

      } else {
        hr        <- hr_series_cm()
        exp_lbl   <- MARKETS[[input$exp_mkt]]$label
        hedge_lbl <- MARKETS[[input$hedge_mkt]]$label
        beta_val <- if (!is.null(hr)) {
          b <- tail(hr$beta[!is.na(hr$beta)], 1L)
          if (length(b) == 0L) "N/A" else sprintf("%.3f", b)
        } else "N/A"
        r2_val <- if (!is.null(hr)) {
          r2 <- tail(hr$r_squared[!is.na(hr$r_squared)], 1L)
          if (length(r2) == 0L) "N/A" else scales::percent(r2, accuracy = 0.1)
        } else "N/A"

        bslib::layout_column_wrap(
          width = 1/2,
          bslib::value_box(
            title    = glue::glue("{exp_lbl} C{input$exp_c_cross} \u2192 {hedge_lbl} C{input$hedge_c_cross} Beta"),
            value    = beta_val,
            showcase = bsicons::bs_icon("sliders")
          ),
          bslib::value_box(
            title    = glue::glue("Cross-Market Hedge Effectiveness (R\u00b2)"),
            value    = r2_val,
            showcase = bsicons::bs_icon("check2-circle")
          )
        )
      }
    })

    # ── Rolling Hedge Ratio chart ─────────────────────────────────────────────
    output$rolling_hr <- plotly::renderPlotly({
      if (isTRUE(input$hr_mode == "per_market")) {
        hrs <- hr_series_list()
        p   <- plotly::plot_ly()
        for (m in names(hrs)) {
          hr  <- hrs[[m]]
          if (is.null(hr)) next
          lbl <- MARKETS[[m]]$label
          col <- MARKETS[[m]]$color
          p <- p %>%
            plotly::add_ribbons(data = hr, x = ~date, ymin = ~ci_lower, ymax = ~ci_upper,
              name        = paste0(lbl, " 95% CI"),
              legendgroup = lbl,
              fillcolor   = paste0(col, "33"),
              line        = list(width = 0)
            ) %>%
            plotly::add_lines(data = hr, x = ~date, y = ~beta,
              name          = paste0(lbl, " Beta"),
              legendgroup   = lbl,
              line          = list(color = col, width = 2),
              hovertemplate = paste0(lbl, " %{x|%Y-%m-%d}: %{y:.3f}<extra></extra>")
            )
        }
        p %>% plotly::layout(
          xaxis  = list(title = ""),
          yaxis  = list(title = glue::glue("C{input$exp_c}/C{input$hedge_c} Hedge Ratio")),
          legend = list(orientation = "h"),
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 1, y1 = 1,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )

      } else {
        hr <- hr_series_cm()
        req(hr)
        exp_lbl   <- MARKETS[[input$exp_mkt]]$label
        hedge_lbl <- MARKETS[[input$hedge_mkt]]$label
        col       <- MARKETS[[input$exp_mkt]]$color

        plotly::plot_ly(hr, x = ~date) %>%
          plotly::add_ribbons(ymin = ~ci_lower, ymax = ~ci_upper, name = "95% CI",
            fillcolor = paste0(col, "33"), line = list(width = 0)) %>%
          plotly::add_lines(y = ~beta, name = "Cross-Market Beta",
            line          = list(color = col, width = 2),
            hovertemplate = "%{x|%Y-%m-%d}: %{y:.3f}<extra></extra>"
          ) %>%
          plotly::layout(
            xaxis  = list(title = ""),
            yaxis  = list(title = glue::glue(
              "{exp_lbl} C{input$exp_c_cross} \u2192 {hedge_lbl} C{input$hedge_c_cross}"
            )),
            legend = list(orientation = "h"),
            shapes = list(list(
              type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 1, y1 = 1,
              line = list(color = "#adb5bd", dash = "dot", width = 1)
            ))
          )
      }
    })

    # ── Term Structure of hedge ratios ────────────────────────────────────────
    output$hr_ts <- plotly::renderPlotly({
      if (isTRUE(input$hr_mode == "per_market")) {
        w_list <- wides_pm()
        mkts   <- names(w_list)
        p      <- plotly::plot_ly()

        for (m in mkts) {
          hrts <- hedge_ratio_term_structure(w_list[[m]], MARKETS[[m]]$rtl_prefix,
                                              exposure_c = input$exp_c,
                                              window     = sel$window())
          if (is.null(hrts)) next
          lbl <- MARKETS[[m]]$label
          col <- MARKETS[[m]]$color

          p <- p %>%
            plotly::add_bars(data = hrts, x = ~contract, y = ~r_squared,
              name          = paste0(lbl, " R\u00b2"),
              legendgroup   = lbl,
              marker        = list(color = paste0(col, "88")),
              yaxis         = "y2",
              hovertemplate = paste0(lbl, " C%{x} R\u00b2: %{y:.2f}<extra></extra>")
            ) %>%
            plotly::add_lines(data = hrts, x = ~contract, y = ~beta,
              name          = paste0(lbl, " Beta"),
              legendgroup   = lbl,
              line          = list(color = col, width = 2.5),
              hovertemplate = paste0(lbl, " C%{x} beta: %{y:.3f}<extra></extra>")
            )
        }

        p %>% plotly::layout(
          barmode = "group",
          xaxis   = list(title = "Hedge contract", dtick = 1),
          yaxis   = list(title = "Hedge ratio (beta)"),
          yaxis2  = list(title = "R\u00b2", overlaying = "y", side = "right",
                         range = c(0, 1)),
          legend  = list(orientation = "h")
        )

      } else {
        req(input$exp_mkt, input$hedge_mkt)
        max_hedge_c <- MARKETS[[input$hedge_mkt]]$max_contracts
        hrts <- hedge_ratio_term_structure_cross_market(
          exp_wide_cm(),   MARKETS[[input$exp_mkt]]$rtl_prefix,   input$exp_c_cross,
          hedge_wide_cm(), MARKETS[[input$hedge_mkt]]$rtl_prefix,
          max_hedge_c = min(12L, max_hedge_c),
          window      = sel$window()
        )
        req(hrts)

        exp_lbl   <- MARKETS[[input$exp_mkt]]$label
        hedge_lbl <- MARKETS[[input$hedge_mkt]]$label
        col       <- MARKETS[[input$exp_mkt]]$color

        plotly::plot_ly(hrts, x = ~contract) %>%
          plotly::add_bars(y = ~r_squared,
            name          = "R\u00b2 (effectiveness)",
            marker        = list(color = "#adb5bd"),
            yaxis         = "y2",
            hovertemplate = paste0(hedge_lbl, " C%{x} R\u00b2: %{y:.2f}<extra></extra>")
          ) %>%
          plotly::add_lines(y = ~beta,
            name          = "Hedge ratio (beta)",
            line          = list(color = col, width = 2.5),
            hovertemplate = paste0(hedge_lbl, " C%{x} beta: %{y:.3f}<extra></extra>")
          ) %>%
          plotly::layout(
            xaxis  = list(title = glue::glue("{hedge_lbl} hedge contract"), dtick = 1),
            yaxis  = list(title = glue::glue("Beta ({exp_lbl} C{input$exp_c_cross} exposure)")),
            yaxis2 = list(title = "R\u00b2", overlaying = "y", side = "right",
                          range = c(0, 1)),
            legend = list(orientation = "h")
          )
      }
    })

    # ── Basis Risk ────────────────────────────────────────────────────────────
    # Per-market: one pair of (unhedged, hedged vol) lines per market.
    #   Solid = unhedged, dashed = hedged. Colour identifies market.
    # Cross-market: single pair for the cross-market position.
    output$basis_risk <- plotly::renderPlotly({
      if (isTRUE(input$hr_mode == "per_market")) {
        hrs    <- hr_series_list()
        w_list <- wides_pm()
        mkts   <- names(hrs)
        p      <- plotly::plot_ly()

        for (m in mkts) {
          hr  <- hrs[[m]]
          w   <- w_list[[m]]
          pfx <- MARKETS[[m]]$rtl_prefix
          lbl <- MARKETS[[m]]$label
          col <- MARKETS[[m]]$color
          if (is.null(hr)) next

          exp_col   <- paste0(pfx, "_C", input$exp_c)
          hedge_col <- paste0(pfx, "_C", input$hedge_c)
          if (!all(c(exp_col, hedge_col) %in% names(w))) next

          x_ret     <- log(w[[exp_col]]   / dplyr::lag(w[[exp_col]]))
          y_ret     <- log(w[[hedge_col]] / dplyr::lag(w[[hedge_col]]))
          last_beta <- tail(hr$beta[!is.na(hr$beta)], 1L)
          if (length(last_beta) == 0L) next

          bd               <- basis_risk_decomposition(x_ret, y_ret, last_beta)
          hedged_ret_series <- y_ret - last_beta * x_ret
          basis_pct        <- scales::percent(bd$basis_risk_pct / 100, accuracy = 0.1)

          basis_df <- data.frame(
            date  = w$date,
            basis = rolling_realized_vol(hedged_ret_series, window = sel$window()),
            total = rolling_realized_vol(y_ret, window = sel$window())
          ) %>% dplyr::filter(!is.na(basis))

          p <- p %>%
            plotly::add_lines(data = basis_df, x = ~date, y = ~total,
              name          = paste0(lbl, " unhedged"),
              legendgroup   = lbl,
              line          = list(color = col, width = 1.5),
              hovertemplate = paste0(lbl, " unhedged %{x|%Y-%m-%d}: %{y:.1%}<extra></extra>")
            ) %>%
            plotly::add_lines(data = basis_df, x = ~date, y = ~basis,
              name          = paste0(lbl, " hedged (basis: ", basis_pct, ")"),
              legendgroup   = lbl,
              line          = list(color = col, width = 1.5, dash = "dash"),
              hovertemplate = paste0(lbl, " hedged %{x|%Y-%m-%d}: %{y:.1%}<extra></extra>")
            )
        }

        p %>% plotly::layout(
          xaxis  = list(title = ""),
          yaxis  = list(title = "Annualised vol", tickformat = ".0%"),
          legend = list(orientation = "h")
        )

      } else {
        hr <- hr_series_cm()
        req(hr)

        exp_pfx   <- MARKETS[[input$exp_mkt]]$rtl_prefix
        hedge_pfx <- MARKETS[[input$hedge_mkt]]$rtl_prefix
        exp_col   <- paste0(exp_pfx,   "_C", input$exp_c_cross)
        hedge_col <- paste0(hedge_pfx, "_C", input$hedge_c_cross)

        if (!exp_col   %in% names(exp_wide_cm()))   return(NULL)
        if (!hedge_col %in% names(hedge_wide_cm()))  return(NULL)

        exp_s   <- dplyr::select(exp_wide_cm(),   date, price = dplyr::all_of(exp_col))
        hedge_s <- dplyr::select(hedge_wide_cm(), date, price = dplyr::all_of(hedge_col))
        merged  <- dplyr::inner_join(exp_s, hedge_s, by = "date",
                                     suffix = c("_exp", "_hedge")) %>%
          dplyr::arrange(date)

        x_ret     <- log(merged$price_exp   / dplyr::lag(merged$price_exp))
        y_ret     <- log(merged$price_hedge / dplyr::lag(merged$price_hedge))
        last_beta <- tail(hr$beta[!is.na(hr$beta)], 1L)
        if (length(last_beta) == 0L) return(NULL)

        bd               <- basis_risk_decomposition(x_ret, y_ret, last_beta)
        hedged_ret_series <- y_ret - last_beta * x_ret
        basis_pct        <- scales::percent(bd$basis_risk_pct / 100, accuracy = 0.1)

        basis_df <- data.frame(
          date  = merged$date,
          basis = rolling_realized_vol(hedged_ret_series, window = sel$window()),
          total = rolling_realized_vol(y_ret,             window = sel$window())
        ) %>% dplyr::filter(!is.na(basis))

        exp_lbl   <- MARKETS[[input$exp_mkt]]$label
        hedge_lbl <- MARKETS[[input$hedge_mkt]]$label
        col       <- MARKETS[[input$exp_mkt]]$color

        plotly::plot_ly(basis_df, x = ~date) %>%
          plotly::add_lines(y = ~total,
            name          = paste0(hedge_lbl, " unhedged"),
            line          = list(color = col, width = 1.5),
            hovertemplate = "%{x|%Y-%m-%d}: %{y:.1%}<extra></extra>"
          ) %>%
          plotly::add_lines(y = ~basis,
            name          = paste0(hedge_lbl, " hedged (basis: ", basis_pct, ")"),
            line          = list(color = col, width = 1.5, dash = "dash"),
            hovertemplate = "%{x|%Y-%m-%d}: %{y:.1%}<extra></extra>"
          ) %>%
          plotly::layout(
            xaxis  = list(title = ""),
            yaxis  = list(title = "Annualised vol", tickformat = ".0%"),
            legend = list(orientation = "h"),
            annotations = list(list(
              x = 0.02, y = 0.97, xref = "paper", yref = "paper",
              text      = glue::glue("Basis risk: {basis_pct} of unhedged risk"),
              showarrow = FALSE, font = list(size = 12)
            ))
          )
      }
    })
  })
}
