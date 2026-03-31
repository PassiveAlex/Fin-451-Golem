# mod_hedge_ratios.R
# Hedge ratio dynamics: rolling OLS beta, hedge effectiveness, term structure, basis risk.

mod_hedge_ratios_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      mod_market_selector_ui(ns("sel")),
      hr(),
      numericInput(ns("exp_c"),   "Exposure contract",    value = 1L, min = 1L, max = 36L),
      numericInput(ns("hedge_c"), "Hedge contract",       value = 3L, min = 2L, max = 36L),
      hr(),
      helpText("Term structure: OLS beta for hedging C1",
               "exposure using C2 through C12 (full history).")
    ),
    bslib::layout_column_wrap(
      width = 1/2,
      bslib::value_box(
        title    = "Current Hedge Ratio (beta)",
        value    = textOutput(ns("beta_label"), inline = TRUE),
        showcase = bsicons::bs_icon("sliders")
      ),
      bslib::value_box(
        title    = "Hedge Effectiveness (R²)",
        value    = textOutput(ns("r2_label"), inline = TRUE),
        showcase = bsicons::bs_icon("check2-circle")
      )
    ),
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Rolling Hedge Ratio",
        plotly::plotlyOutput(ns("rolling_hr"), height = "420px")
      ),
      bslib::nav_panel(
        "Term Structure",
        plotly::plotlyOutput(ns("hr_ts"), height = "420px")
      ),
      bslib::nav_panel(
        "Basis Risk",
        plotly::plotlyOutput(ns("basis_risk"), height = "420px")
      )
    )
  )
}

mod_hedge_ratios_server <- function(id, mkt_data) {
  moduleServer(id, function(input, output, session) {
    sel <- mod_market_selector_server("sel")

    wide <- reactive({
      m <- sel$market()
      req(m, mkt_data()[[m]])
      filter_date_range(mkt_data()[[m]]$wide,
                        sel$date_from(), sel$date_to())
    }) |> bindEvent(sel$market(), sel$date_from(), sel$date_to())

    prefix <- reactive(MARKETS[[sel$market()]]$rtl_prefix)

    # ── Rolling hedge ratio series ────────────────────────────────────────────
    hr_series <- reactive({
      rolling_hedge_ratio_series(
        wide(), prefix(), input$exp_c, input$hedge_c, window = sel$window()
      )
    }) |> bindEvent(wide(), input$exp_c, input$hedge_c, sel$window())

    output$beta_label <- renderText({
      hr <- hr_series()
      if (is.null(hr)) return("N/A")
      b <- tail(hr$beta[!is.na(hr$beta)], 1L)
      if (length(b) == 0L) "N/A" else sprintf("%.3f", b)
    })

    output$r2_label <- renderText({
      hr <- hr_series()
      if (is.null(hr)) return("N/A")
      r2 <- tail(hr$r_squared[!is.na(hr$r_squared)], 1L)
      if (length(r2) == 0L) "N/A" else scales::percent(r2, accuracy = 0.1)
    })

    # ── Rolling Hedge Ratio chart ─────────────────────────────────────────────
    output$rolling_hr <- plotly::renderPlotly({
      hr <- hr_series()
      req(hr)

      plotly::plot_ly(hr, x = ~date) |>
        plotly::add_ribbons(
          ymin = ~ci_lower, ymax = ~ci_upper,
          name = "95% CI",
          fillcolor = paste0(MARKETS[[sel$market()]]$color, "33"),
          line = list(width = 0)
        ) |>
        plotly::add_lines(
          y = ~beta, name = "Beta (hedge ratio)",
          line = list(color = MARKETS[[sel$market()]]$color, width = 2),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.3f}<extra></extra>"
        ) |>
        plotly::add_lines(
          y = ~1, line = list(color = "#adb5bd", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = glue::glue("C{input$exp_c}/C{input$hedge_c} Hedge Ratio")),
          legend = list(orientation = "h")
        )
    })

    # ── Term Structure of hedge ratios ────────────────────────────────────────
    output$hr_ts <- plotly::renderPlotly({
      hrts <- hedge_ratio_term_structure(wide(), prefix(),
                                          exposure_c = input$exp_c,
                                          window     = sel$window())
      req(hrts)

      plotly::plot_ly(hrts, x = ~contract) |>
        plotly::add_bars(
          y = ~r_squared, name = "R² (effectiveness)",
          marker = list(color = "#adb5bd"),
          yaxis = "y2",
          hovertemplate = "C%{x} R²: %{y:.2f}<extra></extra>"
        ) |>
        plotly::add_lines(
          y = ~beta, name = "Hedge ratio (beta)",
          line = list(color = MARKETS[[sel$market()]]$color, width = 2.5),
          hovertemplate = "C%{x} beta: %{y:.3f}<extra></extra>"
        ) |>
        plotly::layout(
          xaxis  = list(title = "Hedge contract", dtick = 1),
          yaxis  = list(title = "Hedge ratio (beta)"),
          yaxis2 = list(title = "R²", overlaying = "y", side = "right",
                        range = c(0, 1)),
          legend = list(orientation = "h")
        )
    }) |> bindEvent(wide(), input$exp_c, sel$window())

    # ── Basis Risk ────────────────────────────────────────────────────────────
    output$basis_risk <- plotly::renderPlotly({
      hr <- hr_series()
      req(hr)

      p_prefix <- prefix()
      exp_col   <- paste0(p_prefix, "_C", input$exp_c)
      hedge_col <- paste0(p_prefix, "_C", input$hedge_c)
      if (!all(c(exp_col, hedge_col) %in% names(wide()))) return(NULL)

      x_ret <- log(wide()[[exp_col]]   / dplyr::lag(wide()[[exp_col]]))
      y_ret <- log(wide()[[hedge_col]] / dplyr::lag(wide()[[hedge_col]]))

      # Use last available beta
      last_beta <- tail(hr$beta[!is.na(hr$beta)], 1L)
      if (length(last_beta) == 0L) return(NULL)

      bd <- basis_risk_decomposition(x_ret, y_ret, last_beta)

      # Rolling residuals (basis) for chart
      hedged_ret_series <- y_ret - last_beta * x_ret
      basis_df <- data.frame(
        date   = wide()$date,
        basis  = rolling_realized_vol(hedged_ret_series, window = sel$window()),
        total  = rolling_realized_vol(y_ret, window = sel$window())
      ) |> dplyr::filter(!is.na(basis))

      plotly::plot_ly(basis_df, x = ~date) |>
        plotly::add_lines(y = ~total, name = "Unhedged vol",
          line = list(color = "#c0392b", width = 1.5),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.1%}<extra></extra>"
        ) |>
        plotly::add_lines(y = ~basis, name = "Hedged (basis) vol",
          line = list(color = MARKETS[[sel$market()]]$color, width = 1.5),
          hovertemplate = "%{x|%Y-%m-%d}: %{y:.1%}<extra></extra>"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Annualised vol", tickformat = ".0%"),
          legend = list(orientation = "h"),
          annotations = list(list(
            x = 0.02, y = 0.97, xref = "paper", yref = "paper",
            text = glue::glue("Basis risk: {scales::percent(bd$basis_risk_pct/100, 0.1)} of unhedged risk"),
            showarrow = FALSE, font = list(size = 12)
          ))
        )
    }) |> bindEvent(wide(), hr_series(), input$exp_c, input$hedge_c, sel$window())
  })
}
