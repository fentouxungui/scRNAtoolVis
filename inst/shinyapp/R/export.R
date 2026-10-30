# Plot export with screen-consistent physical dimensions.
#
# A CSS pixel is 1/96 inch.  Exporting at width = px / 96 inches keeps the
# downloaded figure the same physical size as the on-screen figure, regardless
# of the output DPI.  This avoids the historical "download is 1.6x larger"
# problem caused by units = "px", scale = 5, dpi = 300.
#
# This conversion is paired with renderPlot(res = 96) in mod_plot.R.  Change
# both together: with Shiny's default res = 72, exported text would be 25%
# smaller than the screen.

px_to_in <- function(px) {
  px / 96
}

open_plot_device <- function(file, format, dims, dpi = 300) {
  w <- px_to_in(dims$w)
  h <- px_to_in(dims$h)
  if (identical(format, "pdf")) {
    grDevices::pdf(file, width = w, height = h, onefile = FALSE)
  } else if (identical(format, "svg")) {
    if (requireNamespace("svglite", quietly = TRUE)) {
      svglite::svglite(file, width = w, height = h)
    } else {
      grDevices::svg(file, width = w, height = h)
    }
  } else {
    grDevices::png(
      file,
      width = round(dims$w / 96 * dpi),
      height = round(dims$h / 96 * dpi),
      res = dpi
    )
  }
}

save_plot <- function(st, file, format = c("png", "pdf", "svg"),
                      dims, dpi = 300) {
  format <- match.arg(format)
  if (is.null(st) || !is.null(st$error)) {
    stop("There is no successful plot to save.", call. = FALSE)
  }

  if (identical(st$renderer, "grid")) {
    open_plot_device(file, format, dims, dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    do.call(getExportedValue("scRNAtoolVis", st$fn), st$args)
    return(invisible(file))
  }

  if (identical(st$renderer, "heatmap")) {
    open_plot_device(file, format, dims, dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    ComplexHeatmap::draw(st$plot)
    return(invisible(file))
  }

  # ggplot / gg / cowplot, and any other printable object
  if (!inherits(st$plot, c("ggplot", "gg"))) {
    open_plot_device(file, format, dims, dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(st$plot)
    return(invisible(file))
  }

  if (identical(format, "svg")) {
    open_plot_device(file, format, dims, dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(st$plot)
    return(invisible(file))
  }

  ggplot2::ggsave(
    filename = file,
    plot = st$plot,
    width = px_to_in(dims$w),
    height = px_to_in(dims$h),
    units = "in",
    dpi = dpi
  )
  invisible(file)
}
