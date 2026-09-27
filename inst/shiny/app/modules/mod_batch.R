# Batch processing: multiple images or a ZIP archive

mod_batch_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      fileInput(ns("files"), "Images or ZIP archive", multiple = TRUE,
                accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff", ".zip")),
      actionLink(ns("examples"), "Use the two example images"),
      tags$hr(),
      radioButtons(ns("crop"), "Crop", c("Auto" = "auto", "None" = "none"), inline = TRUE),
      helpText("Manual crop is available in the single-image tab."),
      radioButtons(ns("mode"), "Mode", c("Basic" = "basic", "Advanced" = "advanced"), inline = TRUE),
      conditionalPanel("input.mode == 'basic'", ns = ns,
                       checkboxInput(ns("multiclass_basic"), "Multiclass classification", FALSE)),
      conditionalPanel("input.mode == 'advanced'", ns = ns, advanced_controls(ns)),
      actionButton(ns("run"), "Process batch", class = "btn-primary w-100"),
      tags$hr(),
      downloadButton(ns("dl_csv"), "results.csv", class = "btn-sm w-100 mb-1"),
      downloadButton(ns("dl_zip"), "All results (ZIP)", class = "btn-sm w-100")
    ),
    uiOutput(ns("counts")),
    bslib::card(bslib::card_header("Results (select a row to inspect)"),
                DT::DTOutput(ns("table"))),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(bslib::card_header("Original"), plotOutput(ns("p_orig"), height = "260px")),
      bslib::card(bslib::card_header("Leaf mask"), plotOutput(ns("p_mask"), height = "260px")),
      bslib::card(bslib::card_header("Overlay"), plotOutput(ns("p_overlay"), height = "260px"))
    )
  )
}

mod_batch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    staged <- reactiveVal(NULL)   # list(dir, files)
    batch <- reactiveVal(NULL)

    stage <- function(paths, names) {
      d <- tempfile("leaf_batch_"); input_dir <- file.path(d, "input")
      dir.create(input_dir, recursive = TRUE)
      for (i in seq_along(paths)) {
        if (grepl("\\.zip$", names[i], ignore.case = TRUE)) {
          utils::unzip(paths[i], exdir = input_dir)
        } else {
          file.copy(paths[i], file.path(input_dir, names[i]))
        }
      }
      files <- LeafInjuryR::list_leaf_images(input_dir, recursive = TRUE)
      if (!length(files)) showNotification("No supported images found.", type = "error")
      staged(list(dir = d, files = files)); batch(NULL)
      showNotification(sprintf("%d image(s) ready.", length(files)))
    }
    observeEvent(input$files, {
      names <- make.unique(input$files$name, sep = "_")
      stage(input$files$datapath, names)
    })
    observeEvent(input$examples, {
      p <- LeafInjuryR::leaf_example_images(); stage(p, basename(p))
    })

    observeEvent(input$run, {
      st <- staged(); req(st, length(st$files) > 0)
      args <- analysis_args(input, input$mode == "advanced")
      out_dir <- file.path(st$dir, "output")
      unlink(out_dir, recursive = TRUE)
      n <- length(st$files); failed <- 0
      withProgress(message = "Processing batch", value = 0, {
        cb <- function(i, n, file, status) {
          if (!identical(status, "ok")) failed <<- failed + 1
          incProgress(1 / n, detail = sprintf("processed %d / %d | remaining %d | failed %d",
                                              i, n, n - i, failed))
        }
        b <- tryCatch(
          do.call(LeafInjuryR::analyze_leaf_batch,
                  c(list(images = st$files, output_dir = out_dir, progress = FALSE,
                         callback = cb, crop = input$crop), args)),
          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
        batch(b)
      })
    })

    output$counts <- renderUI({
      st <- staged(); b <- batch()
      n <- if (is.null(st)) 0 else length(st$files)
      done <- if (is.null(b)) 0 else nrow(b$results)
      fail <- if (is.null(b)) 0 else sum(b$results$status != "ok")
      bslib::layout_columns(
        bslib::value_box("Images", n), bslib::value_box("Processed", done),
        bslib::value_box("Remaining", n - done), bslib::value_box("Failed", fail))
    })

    table_data <- reactive({
      b <- batch(); req(b)
      r <- b$results
      data.frame(Filename = r$file,
                 `Healthy (%)` = round(r$healthy_percent, 2),
                 `Injured (%)` = round(r$injured_percent, 2),
                 Quality = r$quality_flag, Status = r$status,
                 Message = ifelse(is.na(r$error_message), r$quality_messages %||% "", r$error_message),
                 check.names = FALSE, stringsAsFactors = FALSE)
    })
    output$table <- DT::renderDT(
      DT::datatable(table_data(), selection = "single", rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE)))

    selected <- reactive({
      b <- batch(); i <- input$table_rows_selected
      req(b, length(i) == 1)
      list(row = b$results[i, ], file = staged()$files[i], out = b$output_dir)
    })
    read_png <- function(path) if (file.exists(path)) EBImage::readImage(path) else NULL
    output$p_orig <- renderPlot({
      s <- selected(); im <- tryCatch(LeafInjuryR::read_leaf(s$file), error = function(e) NULL)
      req(im); LeafInjuryR::plot_leaf_image(im)
    })
    output$p_mask <- renderPlot({
      s <- selected()
      im <- read_png(file.path(s$out, "masks", paste0(s$row$output_stem, "_leaf_mask.png")))
      validate(need(!is.null(im), "No mask (image failed)."))
      LeafInjuryR::plot_leaf_image(EBImage::imageData(im) > 0.5)
    })
    output$p_overlay <- renderPlot({
      s <- selected()
      im <- read_png(file.path(s$out, "overlays", paste0(s$row$output_stem, "_overlay.png")))
      validate(need(!is.null(im), "No overlay (image failed)."))
      LeafInjuryR::plot_leaf_image(im)
    })

    output$dl_csv <- downloadHandler(
      filename = function() "results.csv",
      content = function(file) { req(batch()); LeafInjuryR::write_leaf_results(batch(), file) })
    output$dl_zip <- downloadHandler(
      filename = function() "leaf_batch_results.zip",
      content = function(file) {
        req(batch())
        if (!safe_zip_dir(file, batch()$output_dir))
          showNotification("ZIP creation failed (no zip utility found).", type = "error")
      })
  })
}
