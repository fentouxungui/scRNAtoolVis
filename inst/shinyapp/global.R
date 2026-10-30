# scRNAtoolVis Shiny app -------------------------------------------------------

options(shiny.maxRequestSize = 2048 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(colourpicker)
})

if (!requireNamespace("scRNAtoolVis", quietly = TRUE)) {
  stop("The scRNAtoolVis package must be installed to run this app.",
       call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

app_dir <- system.file("shinyapp", package = "scRNAtoolVis")
if (!nzchar(app_dir)) {
  app_dir <- normalizePath(getwd(), mustWork = FALSE)
}

for (f in c(
  "plot_specs.R",
  "page_docs.R",
  "validate.R",
  "adapters.R",
  "render.R",
  "export.R",
  "codegen.R",
  "mod_dataset.R",
  "mod_deg.R",
  "mod_plot.R"
)) {
  source(file.path(app_dir, "R", f), local = FALSE)
}

PLOT_SPECS <- plot_specs()
PAGE_DOCS <- page_docs()
