#' Launch the scRNAtoolVis Shiny application
#'
#' Starts a local Shiny application for interactive exploration of the
#' plotting functions in \pkg{scRNAtoolVis}. The application accepts a Seurat
#' object uploaded as an \code{.rds} or \code{.qs2} file, exposes the parameters
#' of the package's plotting functions, and supports plot export.
#'
#' The application is an add-on: the existing plotting functions are called
#' unchanged through their exported interfaces. Only the Shiny application
#' files under \code{inst/shinyapp} and this launcher are new.
#'
#' @param port Integer port to listen on. \code{NULL} chooses a free port.
#' @param host Character host to bind to. Defaults to \code{"127.0.0.1"}.
#' @param launch.browser Logical; open the system browser?
#' @param ... Additional arguments passed to \code{shiny::runApp()}.
#'
#' @return Invisibly, the value returned by \code{shiny::runApp()}.
#' @export
#' @examples
#' \dontrun{
#' launchScRNAtoolVis()
#' }
launchScRNAtoolVis <- function(port = NULL,
                               host = "127.0.0.1",
                               launch.browser = TRUE,
                               ...) {
  needed <- c(
    "shiny", "shinydashboard", "DT", "colourpicker", "jsonlite"
  )
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(
      "The scRNAtoolVis Shiny app needs these packages: ",
      paste(missing, collapse = ", "),
      ". Install them with install.packages().",
      call. = FALSE
    )
  }

  app_dir <- system.file("shinyapp", package = "scRNAtoolVis")
  if (!nzchar(app_dir)) {
    stop(
      "Could not find the Shiny app directory. Reinstall scRNAtoolVis.",
      call. = FALSE
    )
  }

  shiny::runApp(
    app_dir,
    port = port,
    host = host,
    launch.browser = launch.browser,
    ...
  )
}
