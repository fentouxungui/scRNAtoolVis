# Dataset upload and summary module.

mod_dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      box(
        width = 6, title = "Load a Seurat object", status = "primary",
        solidHeader = TRUE,
        fileInput(ns("file"), "Upload .rds or .qs2",
                  accept = c(".rds", ".qs2")),
        actionButton(ns("load"), "Load uploaded object", icon = icon("upload")),
        hr(),
        selectInput(
          ns("demo_choice"), "Demo object",
          choices = c(
            "PBMC (htdata.RDS)" = "htdata",
            "Test object (seuratTest.RDS)" = "seuratTest"
          )
        ),
        actionButton(ns("demo"), "Load demo object", icon = icon("flask")),
        helpText("The demo files ship with scRNAtoolVis.")
      ),
      box(
        width = 6, title = "Active identity", status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("identity_ui")),
        helpText(
          "Several plots colour or group cells by the active identity. ",
          "Changing it only affects this browser session."
        ),
        verbatimTextOutput(ns("status"))
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Object summary", status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("summary")),
        br(),
        DTOutput(ns("meta")),
        br(),
        DTOutput(ns("reductions"))
      )
    )
  )
}

mod_dataset_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    obj_r <- reactiveVal(NULL)
    info_r <- reactiveVal(NULL)
    identity_r <- reactiveVal("ident")
    status_r <- reactiveVal("No dataset loaded.")

    load_object <- function(obj, name = NULL) {
      if (!inherits(obj, "Seurat")) {
        stop("The file does not contain a Seurat object.", call. = FALSE)
      }
      info <- dataset_info(obj)
      cols <- info$meta_cols
      default_id <- if ("seurat_clusters" %in% cols) {
        "seurat_clusters"
      } else {
        "ident"
      }
      obj_r(obj)
      identity_r(default_id)
      info_r(info)
      status_r(sprintf(
        "Loaded %s: %d cells x %d genes; default assay %s.",
        name %||% info$name, info$ncell, info$ngene, info$assay
      ))
    }

    observeEvent(input$load, {
      req(input$file)
      tryCatch({
        ext <- tolower(sub(".*\\.", "", input$file$name))
        if (!ext %in% c("rds", "qs2")) {
          stop(
            paste0(
              "Only .rds and .qs2 files are supported. ",
              "Convert legacy .qs files with the qs package and save them as .rds or .qs2."
            ),
            call. = FALSE
          )
        }
        obj <- if (identical(ext, "qs2")) {
          if (!requireNamespace("qs2", quietly = TRUE)) {
            stop("Install the qs2 package to read .qs2 files.", call. = FALSE)
          }
          qs2::qs_read(input$file$datapath)
        } else {
          readRDS(input$file$datapath)
        }
        load_object(obj, input$file$name)
      }, error = function(e) {
        status_r(paste("Load failed:", conditionMessage(e)))
      })
    })

    observeEvent(input$demo, {
      tryCatch({
        choice <- input$demo_choice %||% "htdata"
        file <- if (identical(choice, "seuratTest")) {
          "seuratTest.RDS"
        } else {
          "htdata.RDS"
        }
        path <- system.file("extdata", file, package = "scRNAtoolVis")
        if (!nzchar(path)) {
          alt <- if (identical(file, "htdata.RDS")) {
            "seuratTest.RDS"
          } else {
            "htdata.RDS"
          }
          path <- system.file("extdata", alt, package = "scRNAtoolVis")
        }
        if (!nzchar(path)) {
          stop("No demo Seurat object was found in the installed package.",
               call. = FALSE)
        }
        load_object(readRDS(path), basename(path))
      }, error = function(e) {
        status_r(paste("Demo failed:", conditionMessage(e)))
      })
    })

    output$identity_ui <- renderUI({
      info <- info_r()
      if (is.null(info)) {
        return(helpText("Load a dataset first."))
      }
      selectInput(
        session$ns("active_identity"),
        "Active identity",
        choices = c("ident" = "ident", stats::setNames(info$meta_cols, info$meta_cols)),
        selected = identity_r()
      )
    })

    observeEvent(input$active_identity, {
      if (!is.null(input$active_identity) && nzchar(input$active_identity)) {
        identity_r(input$active_identity)
      }
    })

    output$status <- renderText(status_r())

    output$summary <- renderDT({
      info <- info_r()
      req(info)
      datatable(
        data.frame(
          item = c("Name", "Cells", "Genes", "Default assay", "Seurat version"),
          value = c(info$name, info$ncell, info$ngene, info$assay,
                    info$seurat_version),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      )
    })

    output$meta <- renderDT({
      obj <- obj_r()
      req(obj)
      md <- obj@meta.data
      datatable(
        data.frame(
          column = colnames(md),
          class = vapply(md, function(x) paste(class(x), collapse = "/"),
                         character(1)),
          levels = vapply(md, function(x) {
            if (is.factor(x)) length(levels(x)) else NA_integer_
          }, integer(1)),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "tp", pageLength = 10, scrollX = TRUE)
      )
    })

    output$reductions <- renderDT({
      obj <- obj_r()
      req(obj)
      info <- info_r()
      datatable(
        data.frame(
          reduction = info$reductions,
          dimensions = vapply(info$reductions, function(r) {
            ncol(Seurat::Embeddings(obj, reduction = r))
          }, integer(1)),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 10)
      )
    })

    list(
      object = obj_r,
      info = info_r,
      identity = identity_r
    )
  })
}
