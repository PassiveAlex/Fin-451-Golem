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

    # ── Roll Yield ─────────────────────────────────────────────────────────────
    output$roll_yield_plot <- plotly::renderPlotly({
      ry <- compute_roll_yield(wide(), prefix())
      req(ry)

      plotly::plot_ly(ry, x = ~date, y = ~roll_yield, type = "scatter", mode = "lines",
        line = list(color = MARKETS[[sel$market()]]$color, width = 1.5),
        hovertemplate = "%{x|%Y-%m-%d}: %{y:.1%}<extra></extra>"
      ) |>
        plotly::add_lines(
          y    = ~0, line = list(color = "#adb5bd", dash = "dot", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Annualised roll yield (C1→C2)", tickformat = ".1%"),
          shapes = list(list(
            type = "line", x0 = min(ry$date), x1 = max(ry$date),
            y0 = 0, y1 = 0,
            line = list(color = "#adb5bd", dash = "dot", width = 1)
          ))
        )
    })
  })
}
