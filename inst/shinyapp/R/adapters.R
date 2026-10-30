# Per-function argument adapters.
#
# The original plotting functions are never modified.  These adapters only
# translate the app's UI values into the exact shape each function expects,
# and work around a few historical quirks in the exported API.

adapt_args <- function(spec, args, obj, info, identity) {
  id <- spec$id

  # Drop empty strings that came from "leave blank" controls.
  keep <- !vapply(args, function(x) {
    is.character(x) && length(x) == 1L && !nzchar(x)
  }, logical(1))
  args <- args[keep]

  # The app adds a synthetic identity column so that every function can refer
  # to the active identity by a real metadata column name.
  meta_args <- c(
    "clusterCol", "groupFacet", "id", "split.by", "cell.id",
    "group.by", "sample.name", "celltype.name", "cellType", "clusters"
  )
  for (nm in intersect(meta_args, names(args))) {
    if (identical(args[[nm]], "ident")) {
      args[[nm]] <- ".scRNAtoolVis_ident"
    }
  }

  if (identical(id, "featurePlot")) {
    if (!is.null(args$color) && length(args$color) < 2L) {
      stop("The colour gradient needs at least two colours.", call. = FALSE)
    }
  }

  if (identical(id, "scatterCellPlot") &&
      is.null(args$color) &&
      is.null(args$cell.id)) {
    # scatterCellPlot generates random colours when color = NULL; fix them
    # once so the screen, exports, and copied code agree.  With cell.id the
    # function colours (idents x cell.id) combinations, so leave color = NULL.
    n <- nlevels(Seurat::Idents(obj))
    args$color <- jjAnno::useMyCol("paired", n = n)
  }

  if (identical(id, "clusterCornerAxes")) {
    if (!is.null(args$groupFacet) &&
        !args$groupFacet %in% colnames(obj@meta.data)) {
      args$groupFacet <- NULL
    }
    if (isTRUE(args$addCircle) && !isTRUE(args$addCircle.legacy)) {
      if (is.null(args$cicDelta)) {
        args$cicDelta <- 0.5
      }
    } else {
      args$cicDelta <- NULL
    }
  }

  if (identical(id, "featureCornerAxes")) {
    if (is.null(args$groupFacet) ||
        !args$groupFacet %in% colnames(obj@meta.data)) {
      # featureCornerAxes defaults groupFacet to "orig.ident"; pass an
      # explicit NULL so objects without that column facet by gene only.
      args["groupFacet"] <- list(NULL)
    }
  }

  if (identical(id, "jjDotPlot")) {
    if (!is.null(args$markerGene)) {
      args$gene <- NULL
    }
    if (!is.null(args$tree.pos)) {
      args$tree.pos <- trimws(strsplit(args$tree.pos, ",", fixed = TRUE)[[1]])
      args$tree.pos <- args$tree.pos[nzchar(args$tree.pos)]
      if (length(args$tree.pos) == 0L) {
        args$tree.pos <- NULL
      }
    }
  }

  if (identical(id, "averageHeatmap")) {
    if (!is.null(args$group.by) &&
        args$group.by != ".scRNAtoolVis_ident" &&
        args$group.by %in% colnames(obj@meta.data)) {
      md <- args$object@meta.data
      md[[args$group.by]] <- droplevels(factor(md[[args$group.by]]))
      args$object@meta.data <- md
      args$object <- Seurat::SetIdent(args$object, value = md[[args$group.by]])
    }
    if (isTRUE(args$annoCol) && is.null(args$myanCol)) {
      n <- nlevels(Seurat::Idents(args$object))
      args$myanCol <- jjAnno::useMyCol("paired", n = n)
    }

    # averageHeatmap removes genes whose average expression is zero before
    # matching markGenes; catch that case with a readable message instead of
    # an anno_mark() failure on NA indices.
    if (!is.null(args$markGenes)) {
      bad <- NULL
      tryCatch({
        assays <- args$assays %||% "RNA"
        group_by <- args$group.by %||% "ident"
        slot <- args$slot %||% "data"
        vr <- utils::compareVersion(
          as.character(utils::packageVersion("Seurat")), "5"
        )
        avg <- if (vr == 1) {
          Seurat::AverageExpression(
            args$object, features = args$markGenes,
            group.by = group_by, assays = assays, layer = slot
          )
        } else {
          Seurat::AverageExpression(
            args$object, features = args$markGenes,
            group.by = group_by, assays = assays, slot = slot
          )
        }
        mat <- as.matrix(avg[[1]])
        zero <- rownames(mat)[rowSums(mat) == 0]
        bad <- intersect(args$markGenes, zero)
      }, error = function(e) {
        # If the preflight itself is not possible, let the main call report.
        NULL
      })
      if (length(bad) > 0L) {
        stop(
          "These marked genes have zero average expression and would be ",
          "removed before annotation: ", paste(bad, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  if (identical(id, "tracksPlot")) {
    if (is.data.frame(args$genes)) {
      if (anyNA(args$genes$cluster)) {
        stop("Every gene in the gene table needs a cluster.", call. = FALSE)
      }
    }
  }

  if (identical(id, "jjVolcano")) {
    if (!is.null(args$diffData) && "cluster" %in% names(args$diffData)) {
      n <- length(unique(args$diffData$cluster))
      if (n > 0L) {
        if (is.null(args$tile.col) || length(args$tile.col) < n) {
          args$tile.col <- jjAnno::useMyCol("paired", n = n)
        } else if (length(args$tile.col) != n) {
          args$tile.col <- rep(args$tile.col, length.out = n)
        }
      }
    }
  }

  if (identical(id, "markerVolcano")) {
    if (!is.null(args$markers) && "cluster" %in% names(args$markers)) {
      n <- length(unique(args$markers$cluster))
      if (n > 0L) {
        if (is.null(args$labelCol) || length(args$labelCol) < n) {
          args$labelCol <- jjAnno::useMyCol("paired", n = n)
        } else if (length(args$labelCol) != n) {
          args$labelCol <- rep(args$labelCol, length.out = n)
        }
      }
    }
    if (!is.null(args$facetColor) && identical(toupper(args$facetColor), "NA")) {
      args$facetColor <- NA
    }
  }

  if (identical(id, "cellRatioPlot")) {
    if (!is.null(args$sample.order) && !is.null(args$sample.name)) {
      md <- args$object@meta.data
      md[[args$sample.name]] <- factor(md[[args$sample.name]],
                                       levels = args$sample.order)
      args$object@meta.data <- md
      args$sample.order <- NULL
    }
  }

  if (identical(id, "drawLegend")) {
    if (!is.null(args$cellType) && args$cellType %in% colnames(obj@meta.data)) {
      md <- args$object@meta.data
      md[[args$cellType]] <- droplevels(factor(md[[args$cellType]]))
      args$object@meta.data <- md
    }
    if (is.null(args$plot)) {
      args$plot <- ggplot2::ggplot() + ggplot2::theme_void()
    }
  }

  args
}
