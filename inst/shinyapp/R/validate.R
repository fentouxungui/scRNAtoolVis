# Dataset inspection, argument construction, and validation helpers.

dataset_info <- function(obj) {
  if (is.null(obj)) {
    return(NULL)
  }
  meta_cols <- setdiff(colnames(obj@meta.data), ".scRNAtoolVis_ident")
  genes <- tryCatch(rownames(obj), error = function(e) character(0))
  list(
    name = obj@project.name %||% "Seurat object",
    ncell = ncol(obj),
    ngene = nrow(obj),
    assay = Seurat::DefaultAssay(obj),
    assays = Seurat::Assays(obj),
    reductions = Seurat::Reductions(obj),
    meta_cols = meta_cols,
    genes = genes,
    seurat_version = as.character(utils::packageVersion("Seurat"))
  )
}

prepare_object <- function(obj, identity = "ident") {
  if (is.null(obj)) {
    return(NULL)
  }
  if (is.null(identity) || !nzchar(identity) || identical(identity, "ident")) {
    id <- Seurat::Idents(obj)
  } else if (identity %in% colnames(obj@meta.data)) {
    id <- obj[[identity, drop = TRUE]]
  } else {
    id <- Seurat::Idents(obj)
  }
  id <- droplevels(factor(id))
  obj$.scRNAtoolVis_ident <- id
  Seurat::SetIdent(obj, value = id)
}

parse_gene_table <- function(txt) {
  if (is.null(txt) || !nzchar(trimws(txt))) {
    return(NULL)
  }
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(NULL)
  }
  parts <- strsplit(lines, "[,\t]")
  first <- parts[[1]]
  if (length(first) >= 1L && grepl("^gene$", first[1], ignore.case = TRUE)) {
    parts <- parts[-1]
  }
  if (length(parts) == 0L) {
    return(NULL)
  }
  data.frame(
    gene = trimws(vapply(parts, function(x) x[1], character(1))),
    cluster = trimws(vapply(parts, function(x) {
      if (length(x) >= 2L) x[2] else NA_character_
    }, character(1))),
    stringsAsFactors = FALSE
  )
}

convert_control <- function(ctrl, val) {
  switch(
    ctrl$type,
    reduction = as.character(val),
    features = {
      v <- as.character(val)
      v <- v[nzchar(v)]
      if (length(v) == 0L) NULL else v
    },
    assay = {
      v <- as.character(val)
      if (!nzchar(v)) NULL else v
    },
    metadata = {
      v <- as.character(val)
      if (!nzchar(v)) NULL else v
    },
    numeric = as.numeric(val),
    nullable_numeric = {
      if (length(val) == 0L || is.na(val) || !nzchar(as.character(val))) {
        NULL
      } else {
        as.numeric(val)
      }
    },
    slider = as.numeric(val),
    checkbox = isTRUE(val),
    select = as.character(val),
    color = as.character(val),
    color_vector = {
      v <- trimws(strsplit(as.character(val), ",", fixed = TRUE)[[1]])
      v <- v[nzchar(v)]
      if (length(v) == 0L) NULL else v
    },
    text = {
      v <- as.character(val)
      if (!nzchar(v)) NULL else v
    },
    order = {
      v <- trimws(strsplit(as.character(val), ",", fixed = TRUE)[[1]])
      v <- v[nzchar(v)]
      if (length(v) == 0L) NULL else v
    },
    matrix = {
      v <- suppressWarnings(as.numeric(trimws(
        strsplit(as.character(val), ",", fixed = TRUE)[[1]]
      )))
      v <- v[!is.na(v)]
      if (length(v) == 0L) NULL else v
    },
    gene_table = parse_gene_table(as.character(val)),
    list = {
      v <- as.character(val)
      if (!nzchar(trimws(v))) {
        NULL
      } else {
        jsonlite::fromJSON(v, simplifyVector = FALSE)
      }
    },
    val
  )
}

build_args <- function(spec, input, info, obj, identity, deg = NULL) {
  args <- list()
  fn <- getExportedValue("scRNAtoolVis", spec$fn)
  if ("object" %in% names(formals(fn))) {
    args$object <- obj
  }
  for (ctrl in spec$controls) {
    if (identical(ctrl$type, "deg")) {
      if (is.null(deg)) {
        stop("No DE table is available. Load or compute one on the DEG page.",
             call. = FALSE)
      }
      args[[ctrl$arg]] <- deg
      next
    }
    val <- input[[ctrl$id]]
    if (is.null(val)) {
      if (isTRUE(ctrl$required)) {
        stop(sprintf("%s is required.", ctrl$label), call. = FALSE)
      }
      next
    }
    converted <- convert_control(ctrl, val)
    if (is.null(converted)) {
      if (isTRUE(ctrl$required)) {
        stop(sprintf("%s is required.", ctrl$label), call. = FALSE)
      }
      next
    }
    args[[ctrl$arg]] <- converted
  }
  args
}

validate_args <- function(spec, args, info, obj = NULL) {
  id <- spec$id
  obj <- obj %||% args$object
  if (is.null(obj)) {
    return("No dataset is loaded. Open the Upload & Summary page first.")
  }
  if (is.null(info)) {
    return("Dataset information is unavailable.")
  }

  meta_ok <- function(x, label = "Metadata column") {
    if (is.null(x) || !nzchar(x)) {
      return(NULL)
    }
    if (identical(x, ".scRNAtoolVis_ident")) {
      return(NULL)
    }
    if (!x %in% colnames(obj@meta.data)) {
      return(sprintf("%s '%s' is not in the object metadata.", label, x))
    }
    NULL
  }

  feat_ok <- function(x, genes = info$genes) {
    if (is.null(x) || length(x) == 0L) {
      return(NULL)
    }
    miss <- setdiff(unique(as.character(x)), genes)
    if (length(miss) > 0L) {
      return(sprintf("Features not found: %s",
                     paste(utils::head(miss, 8), collapse = ", ")))
    }
    NULL
  }

  red_ok <- function(x, corner = FALSE) {
    if (is.null(x) || !nzchar(x)) {
      return("Please choose a reduction.")
    }
    if (!x %in% info$reductions) {
      return(sprintf("Reduction '%s' not found. Available: %s",
                     x, paste(info$reductions, collapse = ", ")))
    }
    if (corner && !grepl("^(umap|tsne)", x)) {
      return("Corner-axis plots require a reduction whose name starts with umap or tsne.")
    }
    NULL
  }

  deg_cols <- function(df, need) {
    if (is.null(df) || !is.data.frame(df)) {
      return("A DE table is required.")
    }
    if (nrow(df) == 0L) {
      return("The DE table has no rows.")
    }
    miss <- setdiff(need, colnames(df))
    if (length(miss) > 0L) {
      return(sprintf("DE table is missing columns: %s",
                     paste(miss, collapse = ", ")))
    }
    NULL
  }

  msg <- NULL

  if (identical(id, "featurePlot")) {
    msg <- red_ok(args$dim)
    if (!is.null(msg)) return(msg)
    msg <- feat_ok(args$genes)
    if (!is.null(msg)) return(msg)
    if (is.null(args$genes)) {
      return("Please select at least one gene.")
    }
    return(NULL)
  }

  if (identical(id, "clusterCornerAxes")) {
    msg <- red_ok(args$reduction, corner = TRUE)
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$clusterCol, "Colour column")
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$groupFacet, "Facet column")
    if (!is.null(msg)) return(msg)
    if (!isTRUE(args$noSplit) && is.null(args$groupFacet)) {
      return("Facetting requires a Facet by column.")
    }
    if (identical(args$axes, "one") && is.null(args$groupFacet)) {
      return("axes = 'one' requires a Facet by column.")
    }
    return(NULL)
  }

  if (identical(id, "featureCornerAxes")) {
    msg <- red_ok(args$reduction, corner = TRUE)
    if (!is.null(msg)) return(msg)
    msg <- feat_ok(args$features)
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$groupFacet, "Facet column")
    if (!is.null(msg)) return(msg)
    if (identical(args$axes, "one") && is.null(args$groupFacet)) {
      return("axes = 'one' requires a Facet by column.")
    }
    if (xor(is.null(args$minExp), is.null(args$maxExp))) {
      return("minExp and maxExp must be supplied together.")
    }
    return(NULL)
  }

  if (identical(id, "scatterCellPlot")) {
    msg <- red_ok(args$dim)
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$cell.id, "Cell id column")
    if (!is.null(msg)) return(msg)
    if (!is.null(args$color)) {
      expected <- if (is.null(args$cell.id)) {
        nlevels(Seurat::Idents(obj))
      } else {
        nrow(unique(obj@meta.data[
          , c(".scRNAtoolVis_ident", args$cell.id), drop = FALSE
        ]))
      }
      if (length(args$color) != expected) {
        return(sprintf(
          "Supply %d colours for the current cell.id grouping (or leave the field empty).",
          expected
        ))
      }
    }
    return(NULL)
  }

  if (identical(id, "jjDotPlot")) {
    if (is.null(args$gene) == is.null(args$markerGene)) {
      return("Supply exactly one of Genes or Marker table.")
    }
    if (!is.null(args$assay) && !args$assay %in% info$assays) {
      return(sprintf("Assay '%s' not found. Available: %s",
                     args$assay, paste(info$assays, collapse = ", ")))
    }
    gene_universe <- info$genes
    if (!is.null(args$assay) && args$assay %in% info$assays) {
      gene_universe <- rownames(obj[[args$assay]])
    }
    msg <- meta_ok(args$id, "Group column")
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$split.by, "Split column")
    if (!is.null(msg)) return(msg)
    if (!is.null(args$markerGene)) {
      if (!is.data.frame(args$markerGene) ||
          !"gene" %in% colnames(args$markerGene)) {
        return("Marker table needs a gene column.")
      }
      aes_name <- args$aesGroName %||% "cluster"
      if (!aes_name %in% colnames(args$markerGene)) {
        return(sprintf("Marker table needs a '%s' column.", aes_name))
      }
      if (anyNA(args$markerGene[[aes_name]])) {
        return("Every marker table row needs a cluster.")
      }
      if (isTRUE(args$anno) && !"cluster" %in% colnames(args$markerGene)) {
        return("Annotated dot plots need a cluster column in the marker table.")
      }
      msg <- feat_ok(args$markerGene$gene, gene_universe)
    } else {
      msg <- feat_ok(args$gene, gene_universe)
    }
    if (!is.null(msg)) return(msg)
    if (!is.null(args$plot.margin) && length(args$plot.margin) != 4L) {
      return("Plot margin needs four numbers.")
    }
    if (!is.null(args$tree.pos) && length(args$tree.pos) != 2L) {
      return("Tree positions need two values, e.g. right,top.")
    }
    if (!is.null(args$split.by) && !isTRUE(args$split.by.aesGroup)) {
      n_split <- length(unique(obj@meta.data[[args$split.by]]))
      n_col <- if (is.null(args$dot.col)) 2L else length(args$dot.col)
      if (n_col != n_split) {
        return(sprintf(
          paste0("Split by has %d groups; Dot colours needs %d colours. ",
                 "Alternatively enable 'Colour split dots by expression'."),
          n_split, n_split
        ))
      }
    }
    if ((is.null(args$split.by) || isTRUE(args$split.by.aesGroup)) &&
        !is.null(args$dot.col) && !length(args$dot.col) %in% c(2L, 3L)) {
      return("Dot colours need two or three colours.")
    }
    return(NULL)
  }

  if (identical(id, "averageHeatmap")) {
    if (is.null(args$assays) && !"RNA" %in% info$assays) {
      return("averageHeatmap defaults to the RNA assay; choose an available assay.")
    }
    if (!is.null(args$assays) && !args$assays %in% info$assays) {
      return(sprintf("Assay '%s' not found. Available: %s",
                     args$assays, paste(info$assays, collapse = ", ")))
    }
    if (length(args$markerGene) < 2L) {
      return(paste(
        "Select at least two genes; averageHeatmap needs more than one row",
        "after zero-expression filtering."
      ))
    }
    gene_universe <- info$genes
    assay_names <- args$assays %||% "RNA"
    assay_names <- intersect(assay_names, info$assays)
    if (length(assay_names) > 0L) {
      gene_universe <- unique(unlist(lapply(
        assay_names, function(a) rownames(obj[[a]])
      )))
    }
    msg <- feat_ok(args$markerGene, gene_universe)
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$group.by, "Group column")
    if (!is.null(msg)) return(msg)
    msg <- feat_ok(args$markGenes, gene_universe)
    if (!is.null(msg)) return(msg)
    if (length(args$htCol) != 3L) {
      return("Heatmap colours must contain exactly three colours.")
    }
    if (length(args$htRange) != 3L) {
      return("Colour range must contain exactly three numbers.")
    }
    if (!is.null(args$markGenes) && !is.null(args$markerGene)) {
      miss <- setdiff(args$markGenes, args$markerGene)
      if (length(miss) > 0L) {
        return("Mark genes must also be present in the gene list.")
      }
    }
    if (xor(is.null(args$width), is.null(args$height))) {
      return("Body width and height must be supplied together, or both left empty.")
    }
    if (isTRUE(args$annoCol) && !is.null(args$myanCol) &&
        length(args$myanCol) != nlevels(Seurat::Idents(args$object))) {
      return(sprintf("Supply %d annotation colours.",
                     nlevels(Seurat::Idents(args$object))))
    }
    return(NULL)
  }

  if (identical(id, "tracksPlot")) {
    if (!"RNA" %in% info$assays) {
      return("tracksPlot reads object@assays$RNA@data; this object has no RNA assay.")
    }
    gene_universe <- rownames(obj[["RNA"]])
    if (is.data.frame(args$genes)) {
      if (!all(c("gene", "cluster") %in% colnames(args$genes))) {
        return("Gene table needs gene and cluster columns.")
      }
      return(feat_ok(args$genes$gene, gene_universe))
    }
    msg <- feat_ok(args$genes, gene_universe)
    if (!is.null(msg)) return(msg)
    if (is.null(args$genes)) {
      return("Please select at least one gene.")
    }
    return(NULL)
  }

  if (identical(id, "jjVolcano")) {
    msg <- deg_cols(args$diffData,
                    c("avg_log2FC", "p_val", "p_val_adj", "cluster", "gene"))
    if (!is.null(msg)) return(msg)
    if (!is.null(args$legend.position) &&
        length(args$legend.position) != 2L) {
      return("Legend position needs two numbers, e.g. 0.7,0.9.")
    }
    if (!is.null(args$expand) && length(args$expand) != 2L) {
      return("Polar expansion needs two numbers, e.g. -1,1.")
    }
    if (!is.null(args$aesCol) && length(args$aesCol) != 2L) {
      return("Up/down colours need exactly two colours.")
    }
    return(NULL)
  }

  if (identical(id, "markerVolcano")) {
    return(deg_cols(args$markers,
                    c("avg_log2FC", "cluster", "gene", "pct.1", "pct.2")))
  }

  if (identical(id, "cellRatioPlot")) {
    msg <- meta_ok(args$sample.name, "Sample column")
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$celltype.name, "Cell type column")
    if (!is.null(msg)) return(msg)
    if (!is.null(args$fill.col) &&
        length(args$fill.col) !=
          length(unique(obj@meta.data[[args$celltype.name]]))) {
      return("Supply one colour per cell type, or leave the field empty.")
    }
    return(NULL)
  }

  if (identical(id, "drawLegend")) {
    msg <- meta_ok(args$cellType, "Cell type column")
    if (!is.null(msg)) return(msg)
    msg <- meta_ok(args$clusters, "Cluster column")
    if (!is.null(msg)) return(msg)
    if (!is.null(args$ncol) && !args$ncol %in% c(1, 2)) {
      return("Legend columns must be 1 or 2.")
    }
    if (!is.null(args$col) &&
        length(args$col) != length(unique(obj@meta.data[[args$cellType]]))) {
      return("Supply one colour per cell type, or leave the field empty.")
    }
    return(NULL)
  }

  NULL
}

summarize_args <- function(args) {
  keys <- intersect(
    c("dim", "reduction", "genes", "features", "gene", "markerGene",
      "group.by", "id", "clusterCol", "log2FC.cutoff", "pvalue.cutoff",
      "topGeneN", "topn", "plot.type", "sample.name", "celltype.name"),
    names(args)
  )
  if (length(keys) == 0L) {
    return("")
  }
  parts <- vapply(keys, function(k) {
    v <- args[[k]]
    if (is.data.frame(v)) {
      paste0(k, "=", nrow(v), " rows")
    } else if (length(v) > 3L) {
      paste0(k, "=", length(v), " values")
    } else {
      paste0(k, "=", paste(utils::head(v, 3), collapse = ","))
    }
  }, character(1))
  paste(parts, collapse = " | ")
}
