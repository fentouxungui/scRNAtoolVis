# Rendering backends.
#
# scRNAtoolVis functions do not all return the same kind of object:
#   * ggplot / gg objects      -> print()
#   * ComplexHeatmap objects   -> ComplexHeatmap::draw()
#   * grid-drawing functions   -> call again on the target device
# This file hides that difference from the Shiny module.

compute_plot <- function(spec, args, dims) {
  fn <- getExportedValue("scRNAtoolVis", spec$fn)

  if (identical(spec$renderer, "grid")) {
    # Grid functions draw directly and return NULL.  Do not run them here:
    # draw_plot() runs them once when the plot output renders, and
    # save_plot() runs them once when the user exports.
    return(list(
      plot = NULL,
      renderer = "grid",
      fn = spec$fn,
      args = args,
      error = NULL
    ))
  }

  if (identical(spec$renderer, "heatmap")) {
    # averageHeatmap prints its random annotation colours; keep the app log
    # clean without altering the returned Heatmap object.
    p <- NULL
    utils::capture.output(p <- do.call(fn, args))
  } else {
    p <- do.call(fn, args)
  }
  list(plot = p, renderer = spec$renderer, error = NULL)
}

draw_plot <- function(st) {
  if (identical(st$renderer, "grid")) {
    do.call(getExportedValue("scRNAtoolVis", st$fn), st$args)
    return(invisible(NULL))
  }

  p <- st$plot
  if (inherits(p, c("Heatmap", "HeatmapList"))) {
    ComplexHeatmap::draw(p)
  } else if (inherits(p, c("ggplot", "gg"))) {
    print(p)
  } else {
    print(p)
  }
  invisible(NULL)
}
