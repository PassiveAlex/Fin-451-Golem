# mod_market_selector.R
# Shared input controls. Use show_window / show_contracts to hide controls
# that are irrelevant to a particular tab's sidebar.

mod_market_selector_ui <- function(id,
                                    show_window    = TRUE,
                                    show_contracts = FALSE) {
  ns <- NS(id)
  div(
    class = "sidebar-inputs",
    selectizeInput(
      ns("market"),
      label    = "Market",
      choices  = stats::setNames(
        ENABLED_MARKETS,
        sapply(ENABLED_MARKETS, function(m) MARKETS[[m]]$label)
      ),
      selected = ENABLED_MARKETS[1L],
      multiple = TRUE,
      options  = list(plugins = list("remove_button"))
    ),
    if (show_contracts)
      sliderInput(
        ns("contract_range"),
        label = "Contract range",
        min   = 1L, max = 36L,
        value = c(1L, 12L), step = 1L
      ),
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
    if (show_window)
      radioButtons(
        ns("window"),
        label    = "Rolling window",
        choices  = c("21d" = 21L, "63d" = 63L, "126d" = 126L, "252d" = 252L),
        selected = DEFAULT_WINDOW,
        inline   = TRUE
      )
  )
}

mod_market_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Update contract slider max to min across all selected markets
    observeEvent(input$market, {
      mkts <- input$market
      if (length(mkts) == 0L) return()
      max_c <- min(sapply(mkts, function(m) MARKETS[[m]]$max_contracts))
      cur   <- input$contract_range
      if (!is.null(cur)) {
        updateSliderInput(
          session, "contract_range",
          max   = max_c,
          value = c(1L, min(cur[2L], max_c))
        )
      }
    }, ignoreInit = TRUE)

    observeEvent(input$full_history, {
      updateDateRangeInput(session, "date_range",
                           start = as.Date("2007-01-01"),
                           end   = Sys.Date())
    })

    list(
      market         = reactive(input$market),
      contract_range = reactive(input$contract_range),
      date_from      = reactive(input$date_range[1]),
      date_to        = reactive(input$date_range[2]),
      window         = reactive({
        w <- input$window
        if (is.null(w)) DEFAULT_WINDOW else as.integer(w)
      })
    )
  })
}
