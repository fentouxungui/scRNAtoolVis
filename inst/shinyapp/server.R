# Server assembly.

server <- function(input, output, session) {
  dataset <- mod_dataset_server("dataset")
  deg <- mod_deg_server("deg", dataset$object, dataset$identity)

  output$header_status <- renderUI({
    info <- dataset$info()
    if (is.null(info)) {
      return(tags$span(class = "header-status",
                       tags$span(class = "label label-default",
                                 "No dataset loaded")))
    }
    tags$div(
      class = "header-status",
      tags$strong(info$name),
      tags$span(class = "label label-info",
                sprintf("%s cells x %s genes", info$ncell, info$ngene)),
      tags$span(class = "label label-primary",
                sprintf("assay: %s", info$assay)),
      tags$span(class = "label label-default",
                sprintf("Seurat %s", info$seurat_version))
    )
  })

  for (id in names(PLOT_SPECS)) {
    mod_plot_server(
      id,
      PLOT_SPECS[[id]],
      dataset$object,
      dataset$info,
      dataset$identity,
      deg$table
    )
  }

  output$about_session <- renderPrint({
    utils::sessionInfo()
  })
}
