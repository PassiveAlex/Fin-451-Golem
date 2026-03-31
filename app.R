# fin452golem — development entry point
# Run this file with: Rscript app.R
# Or in RStudio: Ctrl+Shift+L then run_app()

pkgload::load_all(path = ".", helpers = FALSE, attach_testthat = FALSE)
run_app()
