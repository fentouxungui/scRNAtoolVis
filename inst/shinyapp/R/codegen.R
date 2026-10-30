# Generate a runnable R snippet that reproduces the current plot.

code_for <- function(spec, args, identity = NULL) {
  args <- args[names(args) != "object"]

  lines <- vapply(names(args), function(nm) {
    val <- args[[nm]]
    if (nm %in% c("diffData", "markers") && is.data.frame(val)) {
      txt <- "deg_table"
    } else if (is.data.frame(val)) {
      txt <- "marker_table"
    } else if (inherits(val, c("ggplot", "gg"))) {
      txt <- "ggplot2::ggplot() + ggplot2::theme_void()"
    } else {
      txt <- paste(deparse(val, width.cutoff = 60), collapse = "\n")
    }
    paste0("  ", nm, " = ", txt)
  }, character(1))

  needs_obj <- "object" %in% names(
    formals(getExportedValue("scRNAtoolVis", spec$fn))
  )

  code_identity <- identity
  prep_lines <- character(0)

  if (identical(spec$id, "averageHeatmap") &&
      !is.null(args$group.by) &&
      !identical(args$group.by, ".scRNAtoolVis_ident")) {
    # averageHeatmap labels its columns with levels(Idents(object)), so the
    # exported code must set Idents to the same grouping column.
    code_identity <- args$group.by
    prep_lines <- c(
      prep_lines,
      sprintf("seu[[%s]] <- factor(seu[[%s]])",
              deparse(args$group.by), deparse(args$group.by))
    )
  }

  if (identical(spec$id, "drawLegend") &&
      !is.null(args$cellType) &&
      !identical(args$cellType, ".scRNAtoolVis_ident")) {
    prep_lines <- c(
      prep_lines,
      sprintf("seu[[%s]] <- factor(seu[[%s]])",
              deparse(args$cellType), deparse(args$cellType))
    )
  }

  head <- c(
    "library(scRNAtoolVis)",
    "",
    "## For volcano plots, supply the DE table used in the app:",
    "## deg_table <- read.csv(\"your_de_table.csv\")",
    "## For marker/gene tables, supply marker_table with gene/cluster columns:",
    "## marker_table <- read.csv(\"your_marker_table.csv\")",
    prep_lines,
    if (needs_obj) {
      c(
        "## The app adds a synthetic identity column so every function can refer",
        "## to the active identity by a real metadata column name.",
        if (!is.null(code_identity) && nzchar(code_identity) &&
            !identical(code_identity, "ident")) {
          sprintf("seu <- Seurat::SetIdent(seu, value = %s)",
                  deparse(code_identity))
        } else {
          "## seu already carries the intended Idents."
        },
        "seu$.scRNAtoolVis_ident <- Seurat::Idents(seu)"
      )
    } else {
      character(0)
    },
    ""
  )

  body_parts <- c(
    if (needs_obj) "  object = seu" else NULL,
    if (length(lines) > 0L) lines else NULL
  )
  body <- paste0(
    "p <- ", spec$fn, "(\n",
    paste(body_parts, collapse = ",\n"),
    "\n)"
  )

  tail <- if (identical(spec$renderer, "heatmap")) {
    "ComplexHeatmap::draw(p)"
  } else if (identical(spec$renderer, "grid")) {
    paste0("# ", spec$fn, " draws directly on the current device.")
  } else {
    "p"
  }

  paste(c(head, body, "", tail), collapse = "\n")
}
