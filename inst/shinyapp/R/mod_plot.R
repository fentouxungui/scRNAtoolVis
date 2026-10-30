# Generic plot module: one UI/server pair for every plotting function.

mod_plot_ui <- function(id, spec) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        box(
          width = 12, title = spec$title, status = "primary",
          solidHeader = TRUE,
          uiOutput(ns("controls")),
          hr(),
          fluidRow(
            column(6, numericInput(ns("plot_width"), "Width (px)", 800,
                                   min = 200, max = 5000, step = 50)),
            column(6, numericInput(ns("plot_height"), "Height (px)", 600,
                                   min = 200, max = 5000, step = 50))
          ),
          fluidRow(
            column(4, actionButton(ns("size_apply"), "Apply size",
                                   class = "btn-sm")),
            column(4, actionButton(ns("size_square"), "Square",
                                   class = "btn-sm")),
            column(4, actionButton(ns("size_wide"), "16:9",
                                   class = "btn-sm"))
          ),
          hr(),
          actionButton(ns("draw"), "Draw", icon = icon("play"),
                       class = "btn-primary"),
          actionButton(ns("reset"), "Reset", icon = icon("rotate-left")),
          br(), br(),
          actionButton(ns("code"), "Export code", icon = icon("code")),
          textOutput(ns("status"))
        )
      ),
      column(
        width = 8,
        box(
          width = 12, title = spec$title, status = "primary",
          solidHeader = TRUE,
          div(
            class = "plot-toolbar",
            textOutput(ns("run_info"), inline = TRUE),
            uiOutput(ns("downloads_ui"))
          ),
          div(class = "plot-scroll", uiOutput(ns("plot_ui"))),
          helpText(
            "Plots are rendered at the requested pixel size. ",
            "Downloads preserve the same physical size (1 px = 1/96 inch)."
          )
        )
      )
    )
  )
}

# -- controls -----------------------------------------------------------------

num_input <- function(id, label, value, min = NULL, max = NULL, step = NULL) {
  args <- list(inputId = id, label = label, value = value)
  if (!is.null(min)) args$min <- min
  if (!is.null(max)) args$max <- max
  if (!is.null(step)) args$step <- step
  do.call(numericInput, args)
}

control_ui <- function(ns, ctrl, info) {
  id <- ns(ctrl$id)
  label <- ctrl$label

  input <- switch(
    ctrl$type,
    reduction = {
      choices <- info$reductions
      selected <- ctrl$value
      if (length(choices) == 0L) {
        selected <- character(0)
      } else if (is.null(selected) || !selected %in% choices) {
        selected <- choices[1]
      }
      selectInput(id, label, choices = choices, selected = selected)
    },
    assay = {
      choices <- c("(default)" = "", stats::setNames(info$assays, info$assays))
      selected <- ctrl$value %||% ""
      if (!selected %in% choices) selected <- ""
      selectInput(id, label, choices = choices, selected = selected)
    },
    features = {
      selectizeInput(
        id, label,
        choices = utils::head(info$genes, 2000),
        multiple = TRUE,
        options = list(
          placeholder = "Type to search; first 2000 genes listed",
          maxOptions = 1000,
          create = TRUE,
          plugins = list("remove_button")
        )
      )
    },
    metadata = {
      choices <- c(
        "(none)" = "",
        "ident" = "ident",
        stats::setNames(info$meta_cols, info$meta_cols)
      )
      choices <- choices[!duplicated(unname(choices))]
      selected <- ctrl$value %||% ""
      if (!selected %in% unname(choices)) selected <- ""
      selectInput(id, label, choices = choices, selected = selected)
    },
    numeric = num_input(id, label, ctrl$value %||% 0,
                        ctrl$min, ctrl$max, ctrl$step),
    nullable_numeric = num_input(id, label, ctrl$value %||% NA,
                                 ctrl$min, ctrl$max, ctrl$step),
    slider = sliderInput(
      id, label,
      min = ctrl$min %||% 0,
      max = ctrl$max %||% 1,
      value = ctrl$value %||% 0.5,
      step = ctrl$step %||% 0.01
    ),
    checkbox = checkboxInput(id, label, value = isTRUE(ctrl$value)),
    select = selectInput(id, label, choices = ctrl$choices,
                         selected = ctrl$value),
    color = colourpicker::colourInput(
      id, label, value = ctrl$value %||% "#000000"
    ),
    color_vector = textInput(
      id, label, value = ctrl$value %||% "",
      placeholder = "Comma-separated colours"
    ),
    text = textInput(id, label, value = ctrl$value %||% ""),
    order = textInput(
      id, label, value = ctrl$value %||% "",
      placeholder = "Comma-separated values"
    ),
    matrix = textInput(
      id, label, value = ctrl$value %||% "",
      placeholder = "Comma-separated numbers"
    ),
    gene_table = textAreaInput(
      id, label, value = ctrl$value %||% "", rows = 4,
      placeholder = "gene,cluster (one per line)"
    ),
    list = textAreaInput(
      id, label, value = ctrl$value %||% "", rows = 3,
      placeholder = "JSON list, e.g. {}"
    ),
    deg = helpText("Uses the DE table from the Differential Expression page."),
    helpText("Unsupported control.")
  )

  help_text <- ctrl$help
  if (identical(ctrl$type, "features")) {
    help_text <- paste(
      help_text %||% "",
      "Type to search; the first 2000 genes are listed, and any valid gene name can be typed."
    )
  }

  tagList(
    input,
    if (!is.null(help_text) && nzchar(trimws(help_text))) {
      tags$small(class = "help-block", help_text)
    }
  )
}

build_controls <- function(ns, spec, info) {
  if (is.null(info)) {
    return(helpText("Load a dataset first."))
  }
  groups <- c("Basic", "Appearance", "Layout", "Advanced")
  tagList(lapply(groups, function(g) {
    ctls <- Filter(function(x) identical(x$group, g), spec$controls)
    if (length(ctls) == 0L) {
      return(NULL)
    }
    tags$details(
      class = "param-group",
      open = if (identical(g, "Basic")) "" else NULL,
      tags$summary(g),
      lapply(ctls, function(ctrl) control_ui(ns, ctrl, info))
    )
  }))
}

# -- reset --------------------------------------------------------------------

reset_controls <- function(session, spec) {
  for (ctrl in spec$controls) {
    v <- ctrl$value
    switch(
      ctrl$type,
      reduction = updateSelectInput(session, ctrl$id, selected = v %||% ""),
      assay = updateSelectInput(session, ctrl$id, selected = v %||% ""),
      features = updateSelectizeInput(session, ctrl$id, selected = character(0)),
      metadata = updateSelectInput(session, ctrl$id, selected = v %||% ""),
      numeric = updateNumericInput(session, ctrl$id, value = v %||% 0),
      nullable_numeric = updateNumericInput(session, ctrl$id, value = v %||% NA),
      slider = updateSliderInput(session, ctrl$id, value = v %||% 0.5),
      checkbox = updateCheckboxInput(session, ctrl$id, value = isTRUE(v)),
      select = updateSelectInput(session, ctrl$id, selected = v),
      color = colourpicker::updateColourInput(
        session, ctrl$id, value = v %||% "#000000"
      ),
      color_vector = updateTextInput(session, ctrl$id, value = v %||% ""),
      text = updateTextInput(session, ctrl$id, value = v %||% ""),
      order = updateTextInput(session, ctrl$id, value = v %||% ""),
      matrix = updateTextInput(session, ctrl$id, value = v %||% ""),
      gene_table = updateTextAreaInput(session, ctrl$id, value = v %||% ""),
      list = updateTextAreaInput(session, ctrl$id, value = v %||% "")
    )
  }
}

# -- server -------------------------------------------------------------------

mod_plot_server <- function(id, spec, dataset_r, info_r, identity_r, deg_r) {
  force(id)
  force(spec)
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dims <- reactiveVal(list(w = 800, h = 600))
    state <- reactiveVal(NULL)
    last_args <- reactiveVal(NULL)
    status_r <- reactiveVal("Ready.")
    run_info <- reactiveVal("")

    observeEvent(list(dataset_r(), identity_r()), {
      state(NULL)
      last_args(NULL)
      run_info("")
      status_r("Dataset or identity changed; draw again.")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$controls <- renderUI({
      build_controls(ns, spec, info_r())
    })

    output$plot_ui <- renderUI({
      d <- dims()
      plotOutput(
        ns("plot"),
        width = paste0(d$w, "px"),
        height = paste0(d$h, "px")
      )
    })

    output$plot <- renderPlot({
      dims()
      st <- state()
      req(st)
      if (!is.null(st$error)) {
        graphics::plot.new()
        msg <- paste(strwrap(st$error, width = 80), collapse = "\n")
        graphics::text(0.5, 0.5, msg, col = "red", cex = 0.9)
      } else {
        tryCatch(
          draw_plot(st),
          error = function(e) {
            graphics::plot.new()
            msg <- paste(
              strwrap(paste("Render error:", conditionMessage(e)), width = 80),
              collapse = "\n"
            )
            graphics::text(0.5, 0.5, msg, col = "red", cex = 0.9)
          }
        )
      }
    }, res = 96)

    output$downloads_ui <- renderUI({
      st <- state()
      if (is.null(st) || !is.null(st$error)) {
        return(tags$small(class = "text-muted", "Draw a plot to enable downloads."))
      }
      tagList(
        downloadButton(ns("dl_png"), "PNG", class = "btn-sm"),
        downloadButton(ns("dl_pdf"), "PDF", class = "btn-sm"),
        downloadButton(ns("dl_svg"), "SVG", class = "btn-sm")
      )
    })

    safe_int <- function(x, default) {
      x <- suppressWarnings(as.integer(x))
      if (length(x) == 0L || is.na(x)) default else x
    }

    observeEvent(input$size_apply, {
      dims(list(
        w = max(200L, safe_int(input$plot_width, 800L)),
        h = max(200L, safe_int(input$plot_height, 600L))
      ))
    })

    observeEvent(input$size_square, {
      dims(list(w = 800L, h = 800L))
      updateNumericInput(session, "plot_width", value = 800)
      updateNumericInput(session, "plot_height", value = 800)
    })

    observeEvent(input$size_wide, {
      dims(list(w = 960L, h = 540L))
      updateNumericInput(session, "plot_width", value = 960)
      updateNumericInput(session, "plot_height", value = 540)
    })

    observeEvent(input$draw, {
      obj <- dataset_r()
      if (is.null(obj)) {
        status_r("Load a dataset first.")
        return()
      }
      info <- info_r()
      if (is.null(info)) {
        status_r("Dataset information is unavailable.")
        return()
      }

      status_r("Building arguments...")
      built <- tryCatch({
        deg <- if (is.function(deg_r)) deg_r() else NULL
        args <- build_args(spec, input, info, obj, identity_r(), deg)
        fn_formals <- names(formals(getExportedValue("scRNAtoolVis", spec$fn)))
        needs_obj <- "object" %in% fn_formals
        obj2 <- if (needs_obj) prepare_object(obj, identity_r()) else obj
        if (needs_obj) {
          args$object <- obj2
        }
        args <- adapt_args(spec, args, obj2, info, identity_r())
        list(args = args)
      }, error = function(e) {
        list(error = conditionMessage(e))
      })

      if (!is.null(built$error)) {
        state(list(error = built$error, renderer = spec$renderer))
        status_r(paste("Error:", built$error))
        return()
      }

      args <- built$args
      msg <- validate_args(spec, args, info, obj = obj2)
      if (!is.null(msg)) {
        state(list(error = msg, renderer = spec$renderer))
        status_r(paste("Error:", msg))
        return()
      }

      withProgress(message = paste("Rendering", spec$title), value = 0.2, {
        st <- tryCatch(
          compute_plot(spec, args, dims()),
          error = function(e) {
            list(error = conditionMessage(e), renderer = spec$renderer)
          }
        )
        state(st)
        code_args <- args
        code_args$object <- NULL
        last_args(code_args)
        if (!is.null(st$error)) {
          status_r(paste("Error:", st$error))
        } else {
          status_r("Done.")
          run_info(paste(
            format(Sys.time(), "%Y-%m-%d %H:%M"),
            "|", spec$fn, "|", summarize_args(args)
          ))
        }
      })
    })

    output$status <- renderText(status_r())
    output$run_info <- renderText(run_info())

    observeEvent(input$code, {
      if (is.null(last_args())) {
        status_r("Draw a plot first.")
        return()
      }
      showModal(modalDialog(
        title = paste("R code:", spec$title),
        tags$pre(
          style = "max-height: 60vh; overflow: auto;",
          code_for(spec, last_args(), identity_r())
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    })

    observeEvent(input$reset, {
      reset_controls(session, spec)
      state(NULL)
      last_args(NULL)
      dims(list(w = 800L, h = 600L))
      updateNumericInput(session, "plot_width", value = 800)
      updateNumericInput(session, "plot_height", value = 600)
      status_r("Reset to defaults.")
      run_info("")
    })

    export_plot <- function(file, format) {
      st <- state()
      req(st, is.null(st$error))
      tryCatch(
        save_plot(st, file, format, dims()),
        error = function(e) {
          showNotification(
            paste("Export failed:", conditionMessage(e)),
            type = "error"
          )
        }
      )
    }

    output$dl_png <- downloadHandler(
      filename = function() {
        paste0(spec$id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) export_plot(file, "png")
    )

    output$dl_pdf <- downloadHandler(
      filename = function() {
        paste0(spec$id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      },
      content = function(file) export_plot(file, "pdf")
    )

    output$dl_svg <- downloadHandler(
      filename = function() {
        paste0(spec$id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
      },
      content = function(file) export_plot(file, "svg")
    )
  })
}
