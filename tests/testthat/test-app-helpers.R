test_that("export sizing uses screen-consistent inches", {
  source(system.file("shinyapp", "R", "export.R",
                     package = "scRNAtoolVis"), local = TRUE)
  expect_equal(px_to_in(960), 10)
  expect_equal(px_to_in(480), 5)
})

test_that("every plot spec maps to real function arguments", {
  source(system.file("shinyapp", "R", "plot_specs.R",
                     package = "scRNAtoolVis"), local = TRUE)
  specs <- plot_specs()
  expect_true(length(specs) >= 11L)

  for (nm in names(specs)) {
    spec <- specs[[nm]]
    fn <- getExportedValue("scRNAtoolVis", spec$fn)
    args <- vapply(spec$controls, function(x) x$arg, character(1))
    expect_true(
      all(args %in% names(formals(fn))),
      info = paste("Unknown argument in spec:", nm)
    )
    expect_true(
      all(c("id", "fn", "title", "renderer", "controls") %in% names(spec)),
      info = paste("Incomplete spec:", nm)
    )
  }
})
