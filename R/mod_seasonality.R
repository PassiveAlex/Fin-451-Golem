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

    wide <- reactive({
      m <- sel$market()
      req(m, mkt_data()[[m]])
      # Use full history for seasonality — date filter applied internally
      mkt_data()[[m]]$wide
    }) %>% bindEvent(sel$market())

    prefix <- reactive(MARKETS[[sel$market()]]$rtl_prefix)
    unit   <- reactive(MARKETS[[sel$market()]]$unit)

    # ── Monthly return box plots ──────────────────────────────────────────────
    output$monthly_box <- plotly::renderPlotly({
      mr <- compute_monthly_returns(wide(), prefix(), input$contract_n)
      req(mr)

      plotly::plot_ly(mr, x = ~month, y = ~monthly_ret,
        type = "box",
        boxpoints  = "outliers",
        marker     = list(color = MARKETS[[sel$market()]]$color, size = 4),
        line       = list(color = MARKETS[[sel$market()]]$color),
        fillcolor  = paste0(MARKETS[[sel$market()]]$color, "44"),
        hovertemplate = "%{x}: %{y:.2%}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Month", categoryorder = "array",
                       categoryarray = month.abb),
          yaxis = list(title = "Monthly log return", tickformat = ".1%"),
          shapes = list(list(
            type = "line", x0 = -0.5, x1 = 11.5, y0 = 0, y1 = 0,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )
    }) %>% bindEvent(wide(), input$contract_n)

    # ── Seasonal index ────────────────────────────────────────────────────────
    output$seasonal_idx <- plotly::renderPlotly({
      si <- seasonal_index(wide(), prefix(), input$contract_n)
      req(si)

      plotly::plot_ly(si, x = ~month_num) %>%
        plotly::add_ribbons(
          ymin = ~lower, ymax = ~upper,
          name = "IQR (25th–75th)",
          fillcolor = paste0(MARKETS[[sel$market()]]$color, "33"),
          line = list(width = 0)
        ) %>%
        plotly::add_lines(
          y = ~seasonal_index, name = "Mean seasonal index",
          line = list(color = MARKETS[[sel$market()]]$color, width = 2.5),
          hovertemplate = "%{x}: %{y:.3f}<extra></extra>"
        ) %>%
        plotly::add_lines(
          y = ~1, line = list(color = "#adb5bd", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) %>%
        plotly::layout(
          xaxis = list(title = "Month",
                       ticktext = month.abb, tickvals = 1:12),
          yaxis = list(title = "Seasonal index (1.00 = average)"),
          legend = list(orientation = "h")
        )
    }) %>% bindEvent(wide(), input$contract_n)

    # ── Year-over-year overlay ────────────────────────────────────────────────
    output$yoy <- plotly::renderPlotly({
      yoy_df <- yoy_normalized(wide(), prefix(), input$contract_n)
      req(yoy_df)

      # Colour scale: grey for older years, market accent for most recent
      years  <- sort(unique(yoy_df$year))
      n_yrs  <- length(years)
      colors <- colorRampPalette(c("#d0d0d0", MARKETS[[sel$market()]]$color))(n_yrs)

      p <- plotly::plot_ly()
      for (i in seq_along(years)) {
        yr_df <- dplyr::filter(yoy_df, year == years[i])
        p <- plotly::add_lines(p,
          data = yr_df, x = ~day_of_year, y = ~normalized,
          name = as.character(years[i]),
          line = list(color = colors[i],
                      width = if (i == n_yrs) 2.5 else 1),
          hovertemplate = paste0(years[i], " Day %{x}: %{y:.1f}<extra></extra>")
        )
      }

      p %>%
        plotly::layout(
          xaxis = list(title = "Day of year",
                       ticktext = c("Jan","Feb","Mar","Apr","May","Jun",
                                    "Jul","Aug","Sep","Oct","Nov","Dec"),
                       tickvals = c(1,32,60,91,121,152,182,213,244,274,305,335)),
          yaxis = list(title = "Normalised price (Jan 1 = 100)"),
          legend = list(orientation = "h")
        )
    }) %>% bindEvent(wide(), input$contract_n)

    # ── STL decomposition ─────────────────────────────────────────────────────
    output$stl <- plotly::renderPlotly({
      stl_df <- stl_decompose(wide(), prefix(), input$contract_n)
      req(stl_df)

      mkt_color <- MARKETS[[sel$market()]]$color

      p_trend <- plotly::plot_ly(stl_df, x = ~date, y = ~trend,
        type = "scatter", mode = "lines",
        line = list(color = mkt_color, width = 1.5),
        name = "Trend") %>%
        plotly::layout(yaxis = list(title = "Trend"))

      p_seasonal <- plotly::plot_ly(stl_df, x = ~date, y = ~seasonal,
        type = "scatter", mode = "lines",
        line = list(color = "#2ca02c", width = 1.5),
        name = "Seasonal") %>%
        plotly::layout(yaxis = list(title = "Seasonal"))

      p_remainder <- plotly::plot_ly(stl_df, x = ~date, y = ~remainder,
        type = "bar",
        marker = list(color = "#adb5bd"),
        name = "Remainder") %>%
        plotly::layout(yaxis = list(title = "Remainder"))

      plotly::subplot(p_trend, p_seasonal, p_remainder,
                      nrows = 3L, shareX = TRUE, titleY = TRUE) %>%
        plotly::layout(
          xaxis  = list(title = ""),
          legend = list(orientation = "h"),
          showlegend = TRUE
        )
    }) %>% bindEvent(wide(), input$contract_n)
  })
}
