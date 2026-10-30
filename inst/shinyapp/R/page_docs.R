# Human-readable documentation for the About page.
#
# PAGE_DOCS holds the curated explanations.  The exhaustive control list under
# each page is generated from PLOT_SPECS, so new controls show up automatically.

page_docs <- function() {
  list(
    data = list(
      title = "Upload & Summary",
      effect = paste(
        "Loads a Seurat object into this browser session, chooses the active",
        "identity, and summarises assays, reductions, metadata, and versions.",
        "The object is held in memory only; loading another object resets the",
        "session state."
      ),
      params = c(
        "Upload .rds / .qs2" = "A serialized Seurat object. Reading .qs2 needs the optional qs2 package.",
        "Demo object" = "Two small objects bundled with scRNAtoolVis: a PBMC object and a test object.",
        "Active identity" = "Metadata column copied into Idents. Feature Plot, Scatter Cell Plot, Tracks Plot, and Average Heatmap labels follow it.",
        "Object summary" = "Assays, reductions, and metadata column types/levels; use it to find valid group columns."
      ),
      notes = c(
        "Changing the active identity only changes the in-memory object, not the file on disk.",
        "A new dataset clears the DE table and all rendered plots."
      )
    ),

    featurePlot = list(
      title = "Feature Plot",
      effect = paste(
        "Draws a grid of feature-expression scatter panels on a chosen",
        "reduction, one panel per gene. Colour is a low-to-high expression",
        "gradient and each panel gets its own colour key."
      ),
      params = c(
        "Reduction" = "Embedding used for x/y (umap, tsne, pca, ...).",
        "Genes" = "One or more features from the default assay.",
        "Colour quantile" = "Upper quantile used to cap the colour scale; lower values increase contrast for sparse genes.",
        "Colour gradient" = "Comma-separated colours; empty uses the package five-colour default.",
        "Rows / Columns" = "Panel grid; empty lets the function infer a single row or column.",
        "Panel size" = "Relative size of each scatter panel inside its grid cell.",
        "Point size" = "Cell point size.",
        "Remove axes / legend" = "Hide axis ticks/labels and the per-panel colour key.",
        "Add rectangle / strip" = "Draw a panel border and a gene strip above each panel.",
        "Add corner arrows" = "Replace the box with UMAP1/UMAP2 corner arrows; Keep one corner draws them only on the first panel.",
        "Axis labels" = "Optional custom x/y labels; empty uses the reduction name.",
        "Respect layout" = "Keep the requested row/column aspect ratio."
      ),
      notes = c(
        "This function draws with grid and returns no ggplot object; the app re-runs it for display and export."
      )
    ),

    clusterCornerAxes = list(
      title = "Cluster Corner Axes",
      effect = paste(
        "A UMAP/t-SNE scatter coloured by a cluster column, with clean corner",
        "axes instead of a box. It can facet by a metadata column, label",
        "cluster medians, and draw a hull around each cluster."
      ),
      params = c(
        "Reduction" = "Must start with umap or tsne.",
        "Colour by" = "Metadata column used for point colour and legend; ident uses the active identity.",
        "Facet by" = "Metadata column used for facet_wrap; required when No split is off or Corner axes is one.",
        "No split / Facet rows" = "Keep one panel or facet; rows controls the facet layout.",
        "Point size / Aspect ratio" = "Point size and panel width-to-height ratio.",
        "Corner axis length / label distance" = "Length of the corner lines and offset of the UMAP1/UMAP2 labels.",
        "Corner axes" = "Draw both corner axes, or only on the first facet.",
        "Cell type labels" = "Median-position labels for clusters, with size and colour.",
        "Legend / key size" = "Legend position, visibility, and point size in the key.",
        "Theme" = "default or bwCorner.",
        "Add cluster hulls" = "Draw a hull around each cluster; the legacy method exposes bin, smoothing, and expansion controls.",
        "Hull expansion (cicDelta)" = "Expansion distance for the modern hull method; used only when hulls are on.",
        "Hull alpha / line colour / line type" = "Appearance of the cluster hulls."
      ),
      notes = c(
        "The reduction must start with umap or tsne; otherwise the original function does not draw a useful plot.",
        "Facet by is required for faceting and for axes = one."
      )
    ),

    featureCornerAxes = list(
      title = "Feature Corner Axes",
      effect = paste(
        "Gene-expression scatter on a UMAP/t-SNE embedding with corner axes.",
        "Panels are faceted by gene and optionally by a metadata column; low",
        "and high expression map to a two-colour gradient."
      ),
      params = c(
        "Reduction" = "Must start with umap or tsne.",
        "Features" = "Genes to plot.",
        "Facet by" = "Optional metadata column; empty facets by gene only.",
        "Min / Max expression" = "Colour limits; supply both or neither.",
        "Corner axis length / label distance" = "Corner line length and label offset.",
        "Low / High colour" = "Gradient endpoints.",
        "Corner axes" = "Both axes, or only the first group.",
        "Layout columns" = "Number of columns when faceting by gene only.",
        "Point size / arrow type / line colour / text sizes" = "Appearance controls.",
        "Theme" = "default or bwCorner."
      ),
      notes = c(
        "If Facet by is empty the app passes an explicit NULL, so objects without orig.ident still work.",
        "axes = one requires a Facet by column."
      )
    ),

    scatterCellPlot = list(
      title = "Scatter Cell Plot",
      effect = paste(
        "A grid-composed figure: cluster scatter on the left, a horizontal",
        "cell-count bar chart in the middle, and a compact cluster legend on",
        "the right."
      ),
      params = c(
        "Reduction" = "Embedding used for x/y.",
        "Cell id column" = "Optional metadata column summarised in the count/legend table. With cell.id the function colours idents x cell.id combinations, so leave Cluster colours empty.",
        "Cluster colours" = "One colour per identity level; empty uses a fixed paired palette. Do not set this when Cell id column is used.",
        "Remove axes / bar plot" = "Hide axis ticks/labels or the middle bar chart.",
        "Bar width" = "Width of the bar-chart viewport.",
        "Point size / legend point size" = "Scatter and legend point sizes.",
        "Arrow length" = "Length of the corner arrows when axes are removed."
      ),
      notes = c(
        "This function draws directly on the grid device and returns no object; the app re-runs it for display and export.",
        "Without cell.id the colour vector must match the number of identity levels; with cell.id leave it empty."
      )
    ),

    jjDotPlot = list(
      title = "Dot Plot",
      effect = paste(
        "A dot matrix of genes by clusters. Dot size encodes the fraction of",
        "cells expressing the gene; dot fill encodes scaled average expression.",
        "Optional dendrograms order genes and clusters, and a marker table can",
        "add cell-type annotation."
      ),
      params = c(
        "Assay / Slot" = "Expression assay and slot or layer.",
        "Group by" = "Metadata column for the y-axis clusters; ident uses the active identity.",
        "Split by" = "Optional second metadata column; labels become cluster (group).",
        "Colour split dots by expression" = "Off: fill maps to the split groups and needs one colour per group. On: fill maps to scaled average expression.",
        "Genes / Marker table" = "Supply either a gene list or a gene,cluster table, not both; the table can drive cell-type annotation.",
        "Gene / Cluster order" = "Optional explicit ordering.",
        "Point / Tile layer" = "Draw dots or tiles.",
        "Dot colours / Midpoint" = "Two-colour gradient, or three-colour gradient with midpoint.",
        "Scale / col.min / col.max" = "Scale average expression across groups and clamp the range.",
        "Rescale / range" = "Rescale scaled values into a fixed range.",
        "Dot min / max" = "Size range used for the percent-expressing legend.",
        "Dendrograms" = "Y and X dendrograms; tree positions and same-side labels.",
        "Plot margin" = "Top, right, bottom, left margin in cm.",
        "Annotate cell types" = "Use the marker table cluster column to draw annotation segments.",
        "Legend position / titles" = "Legend placement and the two legend titles."
      ),
      notes = c(
        "Exactly one of Genes and Marker table is required.",
        "With Split by and group-coloured dots, Dot colours must contain one colour per split group; the app reports the required count instead of silently replacing your palette."
      )
    ),

    averageHeatmap = list(
      title = "Average Heatmap",
      effect = paste(
        "A ComplexHeatmap of average expression per group. Rows are genes",
        "(z-scored), columns are groups, and a top annotation bar shows group",
        "colours. Optional gene marks and fixed body width/height are supported."
      ),
      params = c(
        "Genes" = "Genes to average and display. Select at least two; a single row breaks the package heatmap function.",
        "Group by" = "Metadata column used by AverageExpression; the app also sets Idents to this column because the function labels columns with levels(Idents).",
        "Assay / Slot" = "Expression assay and slot or layer.",
        "Heatmap colours / Range" = "Three colours and three numeric anchors for the z-score colour map.",
        "Annotation colours" = "Seeded random colours, or a custom vector with type and alpha.",
        "Row title / gene names / side / font size" = "Row annotation and label controls.",
        "Mark genes" = "Genes to label with a side mark; must be a subset of Genes.",
        "Width / Height (cm)" = "Fixed heatmap body size; supply both or neither. Supplying both disables row and column clustering.",
        "Cluster rows / columns / orders" = "Clustering and explicit row or column order."
      ),
      notes = c(
        "At least two genes must remain after zero-expression filtering.",
        "Marked genes must have nonzero average expression in at least one group; otherwise the package removes them and the annotation step fails.",
        "Keep Group by equal to the intended Idents to avoid column-label mismatch."
      )
    ),

    tracksPlot = list(
      title = "Tracks Plot",
      effect = paste(
        "Single-cell expression tracks (one column per cell) or a z-scored",
        "heatmap, faceted by gene and optionally by a gene,cluster table.",
        "Useful for showing gene programmes across clusters."
      ),
      params = c(
        "Plot type" = "track (bars) or heatmap (tiles).",
        "Genes / Gene,cluster table" = "A gene vector, or a table with gene and cluster columns; the table adds cluster facets.",
        "Min / Max z-score" = "Clamp the heatmap colour scale; heatmap mode only.",
        "Cell / Gene order" = "Optional explicit ordering.",
        "Facet / theme / strip parameters" = "Lists forwarded to ggh4x::facet_nested, ggplot2::theme, and ggh4x::strip_nested."
      ),
      notes = c(
        "Reads object@assays$RNA@data, so the object must have an RNA assay.",
        "Heatmap mode z-scores values and clips them to the min/max.",
        "For very large objects, tracksPlot densifies the RNA matrix; use a subset if memory is tight."
      )
    ),

    deg = list(
      title = "DEG Table",
      effect = paste(
        "Load a differential-expression table or compute one with Seurat.",
        "The result feeds the two volcano pages and can be downloaded as CSV."
      ),
      params = c(
        "Source" = "Upload an existing CSV/TSV or compute with Seurat.",
        "Separator" = "Comma, tab, or semicolon for uploaded tables.",
        "Group column" = "Metadata column used for grouping; ident uses the active identity.",
        "Mode" = "FindAllMarkers (one-vs-rest) or FindMarkers (two groups).",
        "Group 1 / Group 2" = "Two levels for FindMarkers.",
        "Test / logFC / min.pct / only positive" = "Seurat differential-expression options.",
        "Demo markers" = "Load the bundled pbmc.markers table for a quick volcano demo."
      ),
      notes = c(
        "Required columns after normalisation: avg_log2FC, p_val, p_val_adj, cluster, gene.",
        "The table is cleared when the dataset or active identity changes."
      )
    ),

    jjVolcano = list(
      title = "Volcano Plot",
      effect = paste(
        "Per-cluster volcano of log2 fold change, with a grey cluster",
        "background, up/down or adjusted-p colouring, top-gene labels, and",
        "optional polar or flipped layouts."
      ),
      params = c(
        "DE table" = "The current DEG table.",
        "log2FC / p-value / adjusted-p cutoffs" = "Filtering and colour thresholds.",
        "Top genes per cluster" = "Number of up and down genes labelled per cluster.",
        "Custom marker genes" = "Optional genes to label instead of the automatic top genes.",
        "Order by" = "Ranking column for top-gene selection.",
        "Colour type" = "Up/down categories, or adjusted-p categories.",
        "Background / point size / up-down colours" = "Appearance controls.",
        "Legend position" = "Two normalized coordinates.",
        "Cluster tile colours" = "One per cluster; empty generates a paired palette.",
        "Polar / expansion / flip" = "Polar coordinates, y expansion, or coord_flip.",
        "Cluster label size" = "Text size for the cluster labels at y = 0."
      ),
      notes = c(
        "The DE table must contain avg_log2FC, p_val, p_val_adj, cluster, and gene.",
        "Custom marker genes can be a comma-separated gene vector."
      )
    ),

    markerVolcano = list(
      title = "Marker Volcano",
      effect = paste(
        "Faceted marker-gene volcano: x is delta percent expressed",
        "(pct.1 - pct.2) and y is log2 fold change. Top positive and negative",
        "genes are labelled per cluster."
      ),
      params = c(
        "DE table" = "The current DEG table.",
        "Top genes per cluster / log2FC cutoff" = "Labelling and threshold lines.",
        "Custom gene labels" = "Label only these genes.",
        "Cluster colours" = "One per cluster; empty generates a paired palette.",
        "Threshold line width / colour" = "Appearance of the dashed cutoff lines.",
        "Label repulsion / nudges" = "ggrepel force and x/y offsets.",
        "Facet border / fill / y label / rows" = "Facet and axis appearance."
      ),
      notes = c(
        "Requires avg_log2FC, cluster, gene, pct.1, and pct.2."
      )
    ),

    cellRatioPlot = list(
      title = "Cell Ratio Plot",
      effect = paste(
        "Stacked columns of cell-type proportions per sample, connected by",
        "alluvial flows. The y-axis is percent within each sample."
      ),
      params = c(
        "Sample column" = "Metadata column for the x-axis samples.",
        "Cell type column" = "Metadata column used for fill and stratum.",
        "Sample order" = "Optional explicit sample order; the app applies it as factor levels.",
        "Column width / flow alpha / flow curve" = "Layout and alluvial appearance.",
        "Cell type colours" = "One per cell type; empty uses a paired palette."
      ),
      notes = c(
        "Both metadata columns are required.",
        "The app works around the function sample.order handling by setting factor levels before the call."
      )
    ),

    drawLegend = list(
      title = "Legend Composer",
      effect = paste(
        "Builds a standalone cell-type legend panel (coloured points plus",
        "labels) and combines it with a blank base canvas using cowplot."
      ),
      params = c(
        "Cell type column" = "Metadata column with cell-type labels; the app converts it to a factor.",
        "Cluster id column" = "Metadata column with cluster ids shown next to the points.",
        "Legend columns" = "One or two columns.",
        "Point colours" = "One per cell type; empty uses hue_pal.",
        "Point / text size" = "Legend point and text sizes."
      ),
      notes = c(
        "The base canvas is currently blank; combine the legend panel with another figure using cowplot."
      )
    )
  )
}

# Exhaustive control labels, grouped by the same Basic/Appearance/Layout/Advanced
# buckets used on the plot pages.
full_control_list <- function(spec) {
  groups <- c("Basic", "Appearance", "Layout", "Advanced")
  tagList(lapply(groups, function(g) {
    ctls <- Filter(function(x) identical(x$group, g), spec$controls)
    if (length(ctls) == 0L) {
      return(NULL)
    }
    tagList(
      tags$strong(g),
      tags$ul(lapply(ctls, function(x) tags$li(x$label)))
    )
  }))
}

about_docs_ui <- function() {
  sections <- list(
    "Data" = c("data"),
    "Embedding" = c("featurePlot", "clusterCornerAxes", "featureCornerAxes",
                    "scatterCellPlot"),
    "Expression" = c("jjDotPlot", "averageHeatmap", "tracksPlot"),
    "Differential" = c("deg", "jjVolcano", "markerVolcano"),
    "Composition" = c("cellRatioPlot"),
    "Composite" = c("drawLegend")
  )

  box(
    width = 12, title = "Page guide", status = "primary", solidHeader = TRUE,
    tags$p(
      "Each entry explains what the page draws and what its main parameters",
      "control. Open ", tags$strong("All controls on this page"),
      " for the exhaustive list generated from the app's parameter specs."
    ),
    tagList(lapply(names(sections), function(section) {
      tagList(
        tags$h4(section),
        lapply(sections[[section]], function(key) {
          doc <- PAGE_DOCS[[key]]
          spec <- PLOT_SPECS[[key]]
          if (is.null(doc)) {
            return(NULL)
          }
          tagList(
            tags$details(
              class = "doc-entry",
              open = if (identical(key, "data")) "" else NULL,
              tags$summary(
                tags$strong(doc$title),
                if (!is.null(spec)) tags$code(spec$fn)
              ),
              tags$p(doc$effect),
              if (length(doc$params) > 0L) {
                tags$table(
                  class = "table table-condensed table-striped",
                  tags$thead(tags$tr(tags$th("Parameter"), tags$th("What it does"))),
                  tags$tbody(lapply(names(doc$params), function(p) {
                    tags$tr(tags$td(p), tags$td(doc$params[[p]]))
                  }))
                )
              },
              if (length(doc$notes) > 0L) {
                tags$div(
                  class = "doc-notes",
                  tags$strong("Notes"),
                  tags$ul(lapply(doc$notes, tags$li))
                )
              },
              if (!is.null(spec)) {
                tags$details(
                  class = "doc-entry doc-controls",
                  tags$summary("All controls on this page"),
                  full_control_list(spec)
                )
              }
            )
          )
        })
      )
    }))
  )
}
