# dev/01_start.R
# One-time project setup. Run this script once when setting up a new development
# environment. Do NOT source this file as part of the app.

## 1. Install golem if needed
# install.packages("golem")

## 2. Install all declared dependencies
# devtools::install_deps()

## 3. Install the package itself in development mode
# devtools::install()

## 4. Confirm RTL is available (installed from GitHub)
# if (!requireNamespace("RTL", quietly = TRUE)) {
#   remotes::install_github("risktoollib/RTL")
# }

## 5. Set up git (if not already done)
# usethis::use_git()

## 6. Verify golem config is readable
# fin452golem::get_golem_config("golem_name")

## ── How to run the app in development ────────────────────────────────────────
# Option A — RStudio shortcut:
#   1. Press Ctrl+Shift+L  (devtools::load_all)
#   2. run_app()
#
# Option B — R console:
#   pkgload::load_all(".")
#   run_app()
#
# Option C — entry-point script:
#   Rscript app.R
