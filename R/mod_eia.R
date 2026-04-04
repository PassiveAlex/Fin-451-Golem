# mod_eia.R
# Standalone EIA fundamentals tab: inventory, production, imports/exports, regional breakdown.
# Data is loaded from a nightly-refreshed feather file (see dev/fetch_eia_data.R).

EIA_COMMODITY_LABELS <- c(
  crude       = "Crude Oil",
  distillate  = "Distillate / Heating Oil",
  gasoline    = "Motor Gasoline",
  natural_gas = "Natural Gas"
)

mod_eia_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Controls",
      selectInput(ns("commodity"), "Commodity",
        choices  = EIA_COMMODITY_LABELS,
        selected = "crude"
      ),
      conditionalPanel(
        condition = paste0("input['", ns("commodity"), "'] === 'crude'"),
        selectInput(ns("series_type"), "Series type",
          choices = c(
            "Stocks"     = "stocks",
            "Production" = "production",
            "Imports"    = "imports",
            "Exports"    = "exports"
          ),
          selected = "stocks"
        )
      ),
      selectInput(ns("area"), "Region", choices = EIA_AREA_CHOICES$crude),
      dateRangeInput(ns("date_range"), "Date range",
        start = Sys.Date() - 365 * 5,
        end   = Sys.Date()
      ),
      actionButton(ns("full_history"), "Full history",
        class = "btn btn-sm btn-outline-secondary w-100 mb-2"
      )
    ),
    # ── Staleness banner (shown only when data is stale or missing) ───────────
    uiOutput(ns("staleness_banner")),
    # ── Value boxes ───────────────────────────────────────────────────────────
    uiOutput(ns("value_boxes")),
    # ── Charts ────────────────────────────────────────────────────────────────
    bslib::navset_card_pill(
      bslib::nav_panel(
        "Inventory / Storage",
        plotly::plotlyOutput(ns("inventory_chart"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "YoY Comparison",
        plotly::plotlyOutput(ns("yoy_chart"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Regional Breakdown",
        plotly::plotlyOutput(ns("regional_chart"), height = "70%", width = "100%")
      ),
      bslib::nav_panel(
        "Production & Trade",
        plotly::plotlyOutput(ns("trade_chart"), height = "70%", width = "100%")
      )
    )
  )
}

mod_eia_server <- function(id, eia_data) {
  moduleServer(id, function(input, output, session) {

    # ── Staleness banner ───────────────────────────────────────────────────────
    output$staleness_banner <- renderUI({
      ed <- eia_data()
      if (is.null(ed$data)) {
        bslib::card(
          class = "bg-danger text-white mb-2",
          bslib::card_body(
            bsicons::bs_icon("exclamation-triangle-fill"), " ",
            "EIA data file not found. Run ", shiny::tags$code("Rscript dev/fetch_eia_data.R"),
            " locally or trigger the GitHub Actions workflow."
          )
        )
      } else if (isTRUE(ed$is_stale)) {
        ts <- if (!is.null(ed$last_updated)) ed$last_updated else "unknown"
        bslib::card(
          class = "bg-warning mb-2",
          bslib::card_body(
            bsicons::bs_icon("clock-history"), " ",
            paste0("Data may be stale (last updated: ", ts, "). ",
                   "The nightly GitHub Actions workflow should refresh this automatically.")
          )
        )
      } else {
        NULL
      }
    })

    # ── Resolve series_type for non-crude commodities ─────────────────────────
    series_type <- reactive({
      if (input$commodity == "crude") input$series_type else "stocks"
    })

    # ── Update area choices when commodity changes ────────────────────────────
    observeEvent(input$commodity, {
      choices <- EIA_AREA_CHOICES[[input$commodity]]
      updateSelectInput(session, "area", choices = choices, selected = choices[[1]])
    }, ignoreInit = TRUE)

    # ── Update area choices for non-stocks crude series ───────────────────────
    observeEvent(list(input$commodity, input$series_type), {
      if (input$commodity == "crude" && input$series_type %in% c("production", "imports", "exports")) {
        updateSelectInput(session, "area", choices = c("U.S." = "US"), selected = "US")
      } else {
        choices <- EIA_AREA_CHOICES[[input$commodity]]
        updateSelectInput(session, "area", choices = choices,
                          selected = if ("US" %in% choices) "US" else choices[[1]])
      }
    }, ignoreInit = TRUE)

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
                           start = as.Date("2000-01-01"), end = Sys.Date())
    })

    # ── Filtered data reactives ───────────────────────────────────────────────

    # Primary series for selected commodity + type + area, date-filtered
    primary_df <- reactive({
      ed <- eia_data()
      req(!is.null(ed$data))
      ed$data %>%
        dplyr::filter(
          commodity   == input$commodity,
          series_type == series_type(),
          area        == input$area,
          date        >= input$date_range[1],
          date        <= input$date_range[2]
        ) %>%
        dplyr::arrange(date)
    })

    # All areas for the current commodity + type (for regional breakdown)
    all_areas_df <- reactive({
      ed <- eia_data()
      req(!is.null(ed$data))
      ed$data %>%
        dplyr::filter(
          commodity   == input$commodity,
          series_type == series_type()
        ) %>%
        dplyr::arrange(date)
    })

    # ── Value boxes ───────────────────────────────────────────────────────────
    output$value_boxes <- renderUI({
      df <- primary_df()
      req(nrow(df) > 0L)
      ed <- eia_data()

      latest    <- tail(df$value, 1L)
      latest_dt <- tail(df$date, 1L)
      units     <- df$units[1L]
      lbl       <- EIA_COMMODITY_LABELS[input$commodity]

      # YoY change
      yr_ago_df  <- dplyr::filter(df, abs(as.numeric(date - (latest_dt - 365))) < 10)
      yr_ago_val <- if (nrow(yr_ago_df) > 0L) yr_ago_df$value[which.min(abs(as.numeric(yr_ago_df$date - (latest_dt - 365))))] else NA_real_
      yoy_pct    <- if (!is.na(yr_ago_val) && yr_ago_val != 0) (latest - yr_ago_val) / abs(yr_ago_val) else NA_real_

      # 5-yr average for the same ISO week
      cur_wk <- lubridate::isoweek(latest_dt)
      ref_5yr <- ed$data %>%
        dplyr::filter(
          commodity   == input$commodity,
          series_type == series_type(),
          area        == input$area,
          lubridate::year(date)   %in% seq(lubridate::year(latest_dt) - 5,
                                           lubridate::year(latest_dt) - 1),
          lubridate::isoweek(date) == cur_wk
        )
      avg_5yr   <- if (nrow(ref_5yr) > 0L) mean(ref_5yr$value, na.rm = TRUE) else NA_real_
      vs_5yr    <- if (!is.na(avg_5yr) && avg_5yr != 0) (latest - avg_5yr) / abs(avg_5yr) else NA_real_

      # Staleness badge
      ts_text <- if (!is.null(ed$last_updated)) {
        paste0("Updated: ", substr(ed$last_updated, 1L, 10L))
      } else "No timestamp"
      ts_theme <- if (isTRUE(ed$is_stale)) "warning" else "success"

      bslib::layout_column_wrap(
        width = 1/4,
        bslib::value_box(
          title    = paste0(lbl, " \u2014 Latest"),
          value    = paste0(formatC(latest, format = "f", digits = 1L, big.mark = ","), " ", units),
          showcase = bsicons::bs_icon("database-fill")
        ),
        bslib::value_box(
          title    = "vs 5-Year Average",
          value    = if (is.na(vs_5yr)) "N/A" else scales::percent(vs_5yr, accuracy = 0.1),
          showcase = bsicons::bs_icon("bar-chart-fill"),
          theme    = if (!is.na(vs_5yr) && vs_5yr > 0.05) "danger"
                     else if (!is.na(vs_5yr) && vs_5yr < -0.05) "success" else "secondary"
        ),
        bslib::value_box(
          title    = "YoY Change",
          value    = if (is.na(yoy_pct)) "N/A" else scales::percent(yoy_pct, accuracy = 0.1),
          showcase = bsicons::bs_icon("arrow-up-down")
        ),
        bslib::value_box(
          title    = "Data Freshness",
          value    = ts_text,
          showcase = bsicons::bs_icon("clock"),
          theme    = ts_theme
        )
      )
    })

    # ── Inventory / Storage chart ──────────────────────────────────────────────
    # Current year vs 5-year min/max band and mean. Classic EIA visualization.
    output$inventory_chart <- plotly::renderPlotly({
      df <- primary_df()
      req(nrow(df) > 10L)

      cur_year <- max(lubridate::year(df$date))
      range_df <- compute_5yr_range(df, current_year = cur_year)
      req(nrow(range_df) > 0L)

      lbl   <- EIA_COMMODITY_LABELS[input$commodity]
      units <- df$units[1L]

      plotly::plot_ly(range_df, x = ~date) %>%
        plotly::add_ribbons(
          ymin = ~range_min, ymax = ~range_max,
          name      = "5-year range",
          fillcolor = "rgba(180,180,180,0.25)",
          line      = list(width = 0)
        ) %>%
        plotly::add_lines(
          y    = ~avg_5yr, name = "5-year avg",
          line = list(color = "#adb5bd", dash = "dash", width = 1.5),
          hovertemplate = paste0("%{x|%Y-%m-%d} 5yr avg: %{y:,.0f} ", units, "<extra></extra>")
        ) %>%
        plotly::add_lines(
          y    = ~value, name = paste(cur_year),
          line = list(color = "#1f77b4", width = 2.5),
          hovertemplate = paste0("%{x|%Y-%m-%d}: %{y:,.0f} ", units, "<extra></extra>")
        ) %>%
        plotly::layout(
          title  = list(text = paste0(lbl, " \u2014 ", input$area, " Stocks vs 5-Year Range")),
          xaxis  = list(title = ""),
          yaxis  = list(title = units),
          legend = list(orientation = "h")
        )
    })

    # ── YoY comparison chart ──────────────────────────────────────────────────
    # All years overlaid, grey-to-blue ramp, most recent highlighted.
    output$yoy_chart <- plotly::renderPlotly({
      df <- primary_df()
      req(nrow(df) > 0L)

      df <- df %>%
        dplyr::mutate(year = lubridate::year(date),
                      week = lubridate::isoweek(date))

      years  <- sort(unique(df$year))
      n_yrs  <- length(years)
      colors <- colorRampPalette(c("#d0d0d0", "#1f77b4"))(n_yrs)
      lbl    <- EIA_COMMODITY_LABELS[input$commodity]
      units  <- df$units[1L]

      p <- plotly::plot_ly()
      for (i in seq_along(years)) {
        yr_df <- dplyr::filter(df, year == years[i])
        p <- plotly::add_lines(p,
          data          = yr_df, x = ~week, y = ~value,
          name          = as.character(years[i]),
          line          = list(color = colors[i], width = if (i == n_yrs) 2.5 else 1),
          hovertemplate = paste0(years[i], " week %{x}: %{y:,.0f} ", units, "<extra></extra>")
        )
      }

      p %>% plotly::layout(
        title  = list(text = paste0(lbl, " \u2014 ", input$area, " Year-over-Year")),
        xaxis  = list(title = "ISO Week", range = c(1, 52)),
        yaxis  = list(title = units),
        legend = list(orientation = "h")
      )
    })

    # ── Regional breakdown chart ──────────────────────────────────────────────
    # Latest reading for each available region as a horizontal bar chart.
    output$regional_chart <- plotly::renderPlotly({
      df <- all_areas_df()
      req(nrow(df) > 0L)

      # Latest value per area
      latest_by_area <- df %>%
        dplyr::group_by(area, area_label) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(dplyr::desc(value))

      req(nrow(latest_by_area) > 0L)
      lbl   <- EIA_COMMODITY_LABELS[input$commodity]
      units <- latest_by_area$units[1L]

      plotly::plot_ly(latest_by_area,
        x             = ~value, y = ~area_label,
        type          = "bar", orientation = "h",
        marker        = list(color = "#1f77b4"),
        hovertemplate = paste0("%{y}: %{x:,.0f} ", units, "<extra></extra>")
      ) %>%
        plotly::layout(
          title  = list(text = paste0(lbl, " \u2014 Regional Breakdown (latest)")),
          xaxis  = list(title = units),
          yaxis  = list(title = "", autorange = "reversed"),
          margin = list(l = 160)
        )
    })

    # ── Production & Trade chart ──────────────────────────────────────────────
    # Crude only: production, imports, exports on the same chart.
    # Non-crude: informational message.
    output$trade_chart <- plotly::renderPlotly({
      ed <- eia_data()
      req(!is.null(ed$data))

      if (input$commodity != "crude") {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title     = list(text = "Production & Trade data is available for Crude Oil only"),
            xaxis     = list(visible = FALSE),
            yaxis     = list(visible = FALSE),
            plot_bgcolor  = "rgba(0,0,0,0)",
            paper_bgcolor = "rgba(0,0,0,0)"
          ))
      }

      trade_types <- c("production", "imports", "exports")
      trade_colors <- c(production = "#2ca02c", imports = "#1f77b4", exports = "#d62728")

      p <- plotly::plot_ly()
      for (tt in trade_types) {
        tdf <- ed$data %>%
          dplyr::filter(commodity == "crude", series_type == tt, area == "US",
                        date >= input$date_range[1], date <= input$date_range[2]) %>%
          dplyr::arrange(date)
        if (nrow(tdf) == 0L) next
        units <- tdf$units[1L]
        p <- plotly::add_lines(p,
          data          = tdf, x = ~date, y = ~value,
          name          = tools::toTitleCase(tt),
          line          = list(color = trade_colors[tt], width = 1.5),
          hovertemplate = paste0(tools::toTitleCase(tt), " %{x|%Y-%m-%d}: %{y:,.0f} ",
                                 units, "<extra></extra>")
        )
      }

      p %>% plotly::layout(
        title     = list(text = "U.S. Crude Oil \u2014 Production, Imports & Exports"),
        xaxis     = list(title = ""),
        yaxis     = list(title = "Mbbl/d"),
        legend    = list(orientation = "h"),
        hovermode = "x unified"
      )
    })
  })
}
