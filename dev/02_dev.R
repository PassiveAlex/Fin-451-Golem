# dev/02_dev.R
# Per-feature development helpers. Source individual lines as needed.
# Do NOT source this file wholesale.

## ── Add a new Shiny module ────────────────────────────────────────────────────
# golem::add_module(name = "module_name")
# Creates R/mod_module_name.R with UI/server stubs.

## ── Add a new helper function file ────────────────────────────────────────────
# golem::add_fct(name = "function_name")
# Creates R/fct_function_name.R

## ── Add a package dependency ──────────────────────────────────────────────────
# usethis::use_package("packagename")
# Adds to DESCRIPTION Imports

## ── Add a suggested dependency ───────────────────────────────────────────────
# usethis::use_package("packagename", type = "Suggests")

## ── Reload and run during development ────────────────────────────────────────
# devtools::load_all()
# run_app()

## ── Enabling a new market (flip enabled = TRUE in global.R, then) ─────────────
# devtools::load_all()
# run_app()

## ── Document and check ────────────────────────────────────────────────────────
# devtools::document()     # regenerate NAMESPACE and man/ from roxygen2
# devtools::check()        # R CMD check

## ── Stage progression ─────────────────────────────────────────────────────────
# Stage 1 complete: CL
# Stage 2: flip BRN enabled = TRUE, add mod_correlation.R, extend mod_forward_curve
# Stage 3: flip HTT enabled = TRUE
# Stage 4: flip HO, RBOB enabled = TRUE — unit conversion (x42) activates
# Stage 5: flip NG enabled = TRUE — camel-hump curve handling activates
# Stage 6: add mod_var_dashboard, mod_roll_calendar, mod_stress_overlay, mod_cross_market
