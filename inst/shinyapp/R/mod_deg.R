# Differential-expression module: upload a table or compute one with Seurat.

mod_deg_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      box(
        width = 5, title = "Differential expression", status = "primary",
        solidHeader = TRUE,
        radioButtons(
          ns("source"), "Source",
          choices = c("Upload table" = "upload",
                      "Compute with Seurat" = "compute"),
          selected = "upload"
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'upload'", ns("source")),
          fileInput(ns("file"), "Upload CSV / TSV",
                    accept = c(".csv", ".tsv", ".txt")),
          selectInput(ns("sep"), "Separator",
                      choices = c("Comma" = ",", "Tab" = "\t",
                                  "Semicolon" = ";")),
          actionButton(ns("load"), "Load table", icon = icon("upload"))
        ),
        conditionalPanel(
          sprintf("input['%s'] == 'compute'", ns("source")),
          uiOutput(ns("group_ui")),
          radioButtons(
            ns("mode"), "Mode",
            choices = c("All clusters (FindAllMarkers)" = "all",
                        "Two groups (FindMarkers)" = "two"),
            selected = "all"
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'two'", ns("mode")),
            uiOutput(ns("group1_ui")),
            uiOutput(ns("group2_ui"))
          ),
          selectInput(
            ns("test"), "Test",
            choices = c("wilcox", "bimod", "t", "negbinom", "poisson", "LR"),
            selected = "wilcox"
          ),
          numericInput(ns("logfc"), "logFC threshold", 0.25, min = 0,
                       step = 0.05),
          numericInput(ns("minpct"), "min.pct", 0.1, min = 0, max = 1,
                       step = 0.05),
          checkboxInput(ns("onlypos"), "Only positive markers", FALSE),
          actionButton(ns("run"), "Run DE analysis", icon = icon("play"),
                       class = "btn-primary")
        ),
        hr(),
        actionButton(ns("demo"), "Load demo markers (pbmc.markers)",
                     icon = icon("flask")),
        verbatimTextOutput(ns("status"))
      ),
      box(
        width = 7, title = "DE result preview", status = "primary",
        solidHeader = TRUE,
        DTOutput(ns("table")),
        br(),
        downloadButton(ns("download"), "Download DE table (CSV)")
      )
    )
  )
}

normalise_deg <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"gene" %in% names(df)) {
    if ("feature" %in% names(df)) {
      df$gene <- df$feature
    } else {
      df$gene <- rownames(df)
    }
  }
  aliases <- list(
    avg_log2FC = c("log2FC", "avg_logFC", "logFC"),
    p_val = c("pvalue", "PValue", "p"),
    p_val_adj = c("padj", "FDR", "adj.P.Val"),
    cluster = c("group", "ident", "celltype")
  )
  for (target in names(aliases)) {
    if (!target %in% names(df)) {
      hit <- intersect(aliases[[target]], names(df))
      if (length(hit) > 0L) {
        names(df)[names(df) == hit[1]] <- target
      }
    }
  }
  df
}

validate_deg <- function(df) {
  required <- c("avg_log2FC", "p_val", "p_val_adj", "cluster", "gene")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0L) {
    stop(sprintf("The DE table is missing columns: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  df
}

mod_deg_server <- function(id, dataset_r, identity_r) {
  moduleServer(id, function(input, output, session) {
    deg_r <- reactiveVal(NULL)
    status_r <- reactiveVal("No DE table loaded.")

    observeEvent(list(dataset_r(), identity_r()), {
      deg_r(NULL)
      status_r("Dataset or identity changed; load or compute a DE table.")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$group_ui <- renderUI({
      obj <- dataset_r()
      req(obj)
      info <- dataset_info(obj)
      selectInput(
        session$ns("group"), "Group column",
        choices = c("ident" = "ident",
                    stats::setNames(info$meta_cols, info$meta_cols)),
        selected = "ident"
      )
    })

    group_values <- reactive({
      obj <- prepare_object(dataset_r(), identity_r())
      req(obj, input$group)
      if (identical(input$group, "ident")) {
        levels(Seurat::Idents(obj))
      } else {
        unique(as.character(obj@meta.data[[input$group]]))
      }
    })

    output$group1_ui <- renderUI({
      selectInput(session$ns("group1"), "Group 1", choices = group_values())
    })

    output$group2_ui <- renderUI({
      selectInput(session$ns("group2"), "Group 2", choices = group_values())
    })

    store_deg <- function(df, note) {
      df <- validate_deg(normalise_deg(df))
      deg_r(df)
      status_r(sprintf("%s: %d rows.", note, nrow(df)))
      df
    }

    observeEvent(input$load, {
      req(input$file)
      tryCatch({
        df <- utils::read.table(
          input$file$datapath,
          header = TRUE,
          sep = input$sep,
          quote = "\"",
          comment.char = "",
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        store_deg(df, "Loaded DE table")
      }, error = function(e) {
        status_r(paste("Load failed:", conditionMessage(e)))
      })
    })

    observeEvent(input$demo, {
      tryCatch({
        e <- new.env()
        utils::data("pbmc.markers", package = "scRNAtoolVis", envir = e)
        if (!exists("pbmc.markers", envir = e)) {
          utils::data("top3pbmc.markers", package = "scRNAtoolVis", envir = e)
        }
        df <- if (exists("pbmc.markers", envir = e)) {
          get("pbmc.markers", envir = e)
        } else {
          get("top3pbmc.markers", envir = e)
        }
        store_deg(df, "Loaded demo markers")
      }, error = function(e) {
        status_r(paste("Demo failed:", conditionMessage(e)))
      })
    })

    observeEvent(input$run, {
      obj <- prepare_object(dataset_r(), identity_r())
      if (is.null(obj)) {
        status_r("Load a dataset first.")
        return()
      }
      if (is.null(input$group)) {
        status_r("Choose a group column.")
        return()
      }
      tryCatch({
        withProgress(message = "Running differential expression", value = 0.2, {
          if (identical(input$mode, "all")) {
            args <- list(
              object = obj,
              test.use = input$test,
              logfc.threshold = input$logfc,
              min.pct = input$minpct,
              only.pos = input$onlypos,
              verbose = FALSE
            )
            if (!identical(input$group, "ident")) {
              args$group.by <- input$group
            }
            df <- do.call(Seurat::FindAllMarkers, args)
            df$gene <- if ("gene" %in% names(df)) df$gene else rownames(df)
          } else {
            req(input$group1, input$group2)
            if (identical(input$group1, input$group2)) {
              stop("Choose two different groups.", call. = FALSE)
            }
            args <- list(
              object = obj,
              ident.1 = input$group1,
              ident.2 = input$group2,
              test.use = input$test,
              logfc.threshold = input$logfc,
              min.pct = input$minpct,
              only.pos = input$onlypos,
              verbose = FALSE
            )
            if (!identical(input$group, "ident")) {
              args$group.by <- input$group
            }
            df <- do.call(Seurat::FindMarkers, args)
            df$gene <- rownames(df)
            df$cluster <- paste(input$group1, "vs", input$group2)
          }
          incProgress(0.7)
          store_deg(df, "Computed DE table")
        })
      }, error = function(e) {
        status_r(paste("DE analysis failed:", conditionMessage(e)))
      })
    })

    output$status <- renderText(status_r())

    output$table <- renderDT({
      df <- deg_r()
      req(df)
      datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, processing = TRUE)
      )
    }, server = TRUE)

    output$download <- downloadHandler(
      filename = function() {
        paste0("scRNAtoolVis_DE_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        df <- deg_r()
        req(df)
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    list(table = deg_r)
  })
}
