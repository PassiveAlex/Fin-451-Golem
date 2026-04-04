#!/usr/bin/env Rscript
# dev/fetch_eia_data.R
# Fetches all EIA fundamental series via RTL::eia2tidy_all and writes
# inst/extdata/eia_fundamentals.feather + a timestamp file.
#
# Run by GitHub Actions nightly; also usable locally:
#   Rscript dev/fetch_eia_data.R
#
# Requires: EIA_API_KEY in .env (local) or GitHub Actions secret (CI).

if (file.exists(".env")) readRenviron(".env")

# Ensure packages are available (GitHub Actions runner starts clean)
pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "RTL", "arrow")
new  <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org", quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

source("R/fct_eia_data.R")

api_key <- Sys.getenv("EIA_API_KEY")
if (nchar(api_key) == 0L) stop("EIA_API_KEY not set.")

message("=== EIA fetch started: ", format(Sys.time(), tz = "UTC"), " UTC ===")

tryCatch({
  df <- build_eia_fundamentals(api_key)

  out_dir <- "inst/extdata"
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  arrow::write_feather(df, file.path(out_dir, "eia_fundamentals.feather"))
  writeLines(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    file.path(out_dir, "eia_fundamentals_last_updated.txt")
  )

  message("Saved ", nrow(df), " rows | ",
          length(unique(df$commodity)), " commodities | ",
          length(unique(paste(df$commodity, df$series_type))), " series types")
  message("=== Done ===")

}, error = function(e) {
  message("FATAL: ", conditionMessage(e))
  quit(status = 1L)
})
