# Dashboard shell.

# Short hover hints for the sidebar.  Wrapping the menu label in
# span(title = ...) gives the native browser tooltip, matching the approach
# used by SeuratExplorer without adding another dependency.
tooltips <- c(
  data = "Upload a Seurat object (.rds/.qs2), load a demo object, and inspect metadata and reductions.",
  featurePlot = "Plot expression of one or more genes on a UMAP/t-SNE/PCA embedding.",
  clusterCornerAxes = "UMAP/t-SNE cluster map with clean corner axes; optional facets and cluster hulls.",
  featureCornerAxes = "Gene expression on a UMAP/t-SNE embedding with corner axes and optional faceting.",
  scatterCellPlot = "Cell scatter plot with a cell-count bar chart and a compact cluster legend.",
  jjDotPlot = "Dot matrix of gene expression: dot size = percent expressing, colour = average expression.",
  averageHeatmap = "Heatmap of average gene expression per cluster (ComplexHeatmap).",
  tracksPlot = "Single-cell expression tracks or heatmap for selected genes, faceted by cluster.",
  deg = "Upload a DE table or compute FindAllMarkers/FindMarkers with Seurat.",
  jjVolcano = "Per-cluster volcano plot of log2 fold change with adjusted-p colouring.",
  markerVolcano = "Marker-gene volcano plot of delta percent expressed vs log2 fold change.",
  cellRatioPlot = "Alluvial/stacked column plot of cell-type proportions across samples.",
  drawLegend = "Compose a cell-type legend panel and combine it with a base canvas.",
  about = "Version, session information, and how this add-on is structured."
)

menu_divider <- function() {
  tags$li(
    style = paste0(
      "list-style: none; height: 0; margin: 0 12px; padding: 0; ",
      "border-top: 1px solid rgba(255, 255, 255, 0.15);"
    )
  )
}

plot_tab <- function(id) {
  tabItem(tabName = id, mod_plot_ui(id, PLOT_SPECS[[id]]))
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "scRNAtoolVis Explorer",
    titleWidth = 300,
    tags$li(class = "dropdown", uiOutput("header_status"))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        span(title = tooltips[["data"]], "Upload & Summary"),
        tabName = "data", icon = icon("database")
      ),
      menu_divider(),
      menuItem(
        span(title = tooltips[["featurePlot"]], "Feature Plot"),
        tabName = "featurePlot", icon = icon("braille")
      ),
      menuItem(
        span(title = tooltips[["clusterCornerAxes"]], "Cluster Corner Axes"),
        tabName = "clusterCornerAxes", icon = icon("project-diagram")
      ),
      menuItem(
        span(title = tooltips[["featureCornerAxes"]], "Feature Corner Axes"),
        tabName = "featureCornerAxes", icon = icon("project-diagram")
      ),
      menuItem(
        span(title = tooltips[["scatterCellPlot"]], "Scatter Cell Plot"),
        tabName = "scatterCellPlot", icon = icon("braille")
      ),
      menu_divider(),
      menuItem(
        span(title = tooltips[["jjDotPlot"]], "Dot Plot"),
        tabName = "jjDotPlot", icon = icon("dot-circle")
      ),
      menuItem(
        span(title = tooltips[["averageHeatmap"]], "Average Heatmap"),
        tabName = "averageHeatmap", icon = icon("th")
      ),
      menuItem(
        span(title = tooltips[["tracksPlot"]], "Tracks Plot"),
        tabName = "tracksPlot", icon = icon("chart-bar")
      ),
      menu_divider(),
      menuItem(
        span(title = tooltips[["deg"]], "DEG Table"),
        tabName = "deg", icon = icon("table")
      ),
      menuItem(
        span(title = tooltips[["jjVolcano"]], "Volcano Plot"),
        tabName = "jjVolcano", icon = icon("fire")
      ),
      menuItem(
        span(title = tooltips[["markerVolcano"]], "Marker Volcano"),
        tabName = "markerVolcano", icon = icon("fire")
      ),
      menu_divider(),
      menuItem(
        span(title = tooltips[["cellRatioPlot"]], "Cell Ratio Plot"),
        tabName = "cellRatioPlot", icon = icon("chart-pie")
      ),
      menuItem(
        span(title = tooltips[["drawLegend"]], "Legend Composer"),
        tabName = "drawLegend", icon = icon("list")
      ),
      menu_divider(),
      menuItem(
        span(title = tooltips[["about"]], "About"),
        tabName = "about", icon = icon("info-circle")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
    ),
    tabItems(
      tabItem(tabName = "data", mod_dataset_ui("dataset")),
      plot_tab("featurePlot"),
      plot_tab("clusterCornerAxes"),
      plot_tab("featureCornerAxes"),
      plot_tab("scatterCellPlot"),
      plot_tab("jjDotPlot"),
      plot_tab("averageHeatmap"),
      plot_tab("tracksPlot"),
      tabItem(tabName = "deg", mod_deg_ui("deg")),
      plot_tab("jjVolcano"),
      plot_tab("markerVolcano"),
      plot_tab("cellRatioPlot"),
      plot_tab("drawLegend"),
      tabItem(
        tabName = "about",
        about_docs_ui(),
        box(
          width = 12, title = "Session information", status = "primary",
          solidHeader = TRUE,
          tags$p(
            "This Shiny application is an add-on to ",
            tags$strong("scRNAtoolVis"), ". The existing plotting functions are",
            " called unchanged; the app only adds ",
            tags$code("inst/shinyapp"), " and the ",
            tags$code("launchScRNAtoolVis()"), " launcher."
          ),
          verbatimTextOutput("about_session")
        )
      )
    )
  )
)
