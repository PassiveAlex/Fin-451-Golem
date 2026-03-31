#' Launch the Energy Commodity Risk Dashboard
#'
#' @param ... Arguments passed to shiny::shinyApp (e.g., host, port, test)
#'
#' @importFrom shiny shinyApp
#' @export
run_app <- function(...) {
  with_golem_options(
    app = shiny::shinyApp(
      ui     = app_ui(),
      server = app_server,
      ...
    ),
    golem_opts = list()
  )
}

#' Thin wrapper around golem options (works without golem installed)
#' @noRd
with_golem_options <- function(app, golem_opts = list()) {
  if (requireNamespace("golem", quietly = TRUE)) {
    golem::with_golem_options(app, golem_opts = golem_opts)
  } else {
    app
  }
}
