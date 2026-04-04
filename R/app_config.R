#' Application system file helper
#'
#' Resolves a path relative to inst/app/ inside the installed package.
#' Use this instead of hard-coded file.path() calls so the app works both
#' during development (pkgload::load_all) and when installed.
#'
#' @param ... Path components passed to system.file()
#' @return Absolute path to the file
#' @export
app_sys <- function(...) {
  system.file(..., package = "fin452golem", mustWork = FALSE)
}

#' Read a golem config value
#'
#' Wraps config::get() with the golem config file.
#'
#' @param value  Name of the config option to read
#' @param config Configuration to use (default: Sys.getenv("R_CONFIG_ACTIVE", "default"))
#' @param use_parent Logical — whether to inherit from parent config
#' @return The config value
#' @export
get_golem_config <- function(value,
                              config  = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                              use_parent = TRUE) {
  config::get(
    value      = value,
    config     = config,
    file       = app_sys("golem-config.yml"),
    use_parent = use_parent
  )
}
