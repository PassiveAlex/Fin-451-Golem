# Tests recommended by the Golem framework.
# Run with: devtools::test()

test_that("app_sys returns a valid path", {
  expect_true(nchar(app_sys("golem-config.yml")) > 0)
})

test_that("golem-config is readable", {
  expect_equal(get_golem_config("golem_name"), "fin452golem")
})

test_that("run_app returns a shiny.appobj", {
  skip_on_ci()
  app <- run_app(test = TRUE)
  expect_s3_class(app, "shiny.appobj")
})
