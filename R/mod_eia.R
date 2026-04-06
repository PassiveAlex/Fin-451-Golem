# mod_eia.R
# EIA fundamentals tab: Commodity → Series Type → Region selectors.
# Cascading inputs mirror the Forward Curves EIA overlay layout.
# Data loaded from inst/extdata/eia_fundamentals.feather (nightly refresh).
# Defaults: Crude Oil / Consumption / U.S.

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
        choices = c(
          "Crude Oil"                = "crude",
          "Distillate / Heating Oil" = "distillate",
          "Motor Gasoline"           = "gasoline",
          "Natural Gas"              = "natural_gas"
        ),
        selected = "crude"
      ),
      selectInput(ns("series_type"), "Series",
        choices  = EIA_SERIES_TYPE_CHOICES$crude,
        selected = "consumption"
      ),
      selectInput(ns("area"), "Region",
        choices  = EIA_AREA_CHOICES$crude$consumption,
        selected = "US"
      ),
      hr(),
      dateRangeInput(ns("date_range"), "Date range",
        start = Sys.Date() - 365 * 5,
        end   = Sys.Date()
      ),
      actionButton(ns("full_history"), "Full history",
        class = "btn btn-sm btn-outline-secondary w-100 mb-2"
      )
    ),
    uiOutput(ns("staleness_banner")),
    plotly::plotlyOutput(ns("main_chart"), height = "500px", width = "100%")
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
            "EIA data file not found. Run ",
            shiny::tags$code("Rscript dev/fetch_eia_data.R"), " locally."
          )
        )
      } else if (isTRUE(ed$is_stale)) {
        bslib::card(
          class = "bg-warning mb-2",
          bslib::card_body(
            bsicons::bs_icon("clock-history"), " ",
            paste0("Data may be stale (last updated: ", ed$last_updated, ").")
          )
        )
      } else {
        NULL
      }
    })

    # ── Cascading selectors ────────────────────────────────────────────────────

    # When commodity changes: update Series choices, then Area choices.
    observeEvent(input$commodity, {
      st_choices <- EIA_SERIES_TYPE_CHOICES[[input$commodity]]
      # Keep current series selection if it's valid for the new commodity
      cur_st <- if (input$series_type %in% st_choices) input$series_type else st_choices[[1L]]
      updateSelectInput(session, "series_type", choices = st_choices, selected = cur_st)

      area_choices <- EIA_AREA_CHOICES[[input$commodity]][[cur_st]]
      if (is.null(area_choices)) area_choices <- c("U.S." = "US")
      cur_area <- if (input$area %in% area_choices) input$area else "US"
      updateSelectInput(session, "area", choices = area_choices, selected = cur_area)
    }, ignoreInit = TRUE)

    # When series type changes: update Area choices only.
    observeEvent(input$series_type, {
      area_choices <- EIA_AREA_CHOICES[[input$commodity]][[input$series_type]]
      if (is.null(area_choices)) area_choices <- c("U.S." = "US")
      cur_area <- if (input$area %in% area_choices) input$area else "US"
      updateSelectInput(session, "area", choices = area_choices, selected = cur_area)
    }, ignoreInit = TRUE)

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
        start = as.Date("2000-01-01"), end = Sys.Date()
      )
    })

    # ── Filtered data reactive ─────────────────────────────────────────────────
    primary_df <- reactive({
      ed <- eia_data()
      req(!is.null(ed$data))
      req(input$commodity, input$series_type, input$area)

      df <- ed$data %>%
        dplyr::filter(
          commodity   == input$commodity,
          series_type == input$series_type,
          area        == input$area,
          date        >= input$date_range[1],
          date        <= input$date_range[2]
        )

      # Prefer weekly when both frequencies exist for this series
      if ("frequency" %in% names(df) &&
          "weekly" %in% df$frequency &&
          "monthly" %in% df$frequency) {
        df <- dplyr::filter(df, frequency == "weekly")
      }

      dplyr::arrange(df, date)
    })

    # ── Main chart ─────────────────────────────────────────────────────────────
    output$main_chart <- plotly::renderPlotly({
      df <- primary_df()
      req(nrow(df) > 0L)

      comm_lbl  <- EIA_COMMODITY_LABELS[[input$commodity]]
      series_lbl <- names(EIA_SERIES_TYPE_CHOICES[[input$commodity]])[
        EIA_SERIES_TYPE_CHOICES[[input$commodity]] == input$series_type
      ]
      area_lbl  <- df$area_label[[1L]]
      units     <- df$units[[1L]]

      plotly::plot_ly(
        data          = df,
        x             = ~date,
        y             = ~value,
        type          = "scatter",
        mode          = "lines",
        line          = list(color = "#1f77b4", width = 2),
        hovertemplate = paste0("%{x|%Y-%m-%d}: %{y:,.1f} ", units, "<extra></extra>")
      ) %>%
        plotly::layout(
          title  = list(text = paste0(comm_lbl, " \u2014 ", series_lbl,
                                      " \u2014 ", area_lbl)),
          xaxis  = list(title = ""),
          yaxis  = list(title = units)
        )
    })

  })
}
