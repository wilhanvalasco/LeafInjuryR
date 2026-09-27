# Single-image analysis: Upload -> Crop -> Analyze -> Inspect -> Export

mod_single_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      tags$h6("1. Upload"),
      fileInput(ns("file"), NULL, accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff")),
      actionLink(ns("example1"), "Use example image 1"), " | ",
      actionLink(ns("example2"), "example image 2"),
      tags$hr(),
      tags$h6("2. Crop"),
      radioButtons(ns("crop"), NULL, c("Auto" = "auto", "Manual" = "manual", "None" = "none"),
                   inline = TRUE),
      conditionalPanel("input.crop == 'manual'", ns = ns,
                       helpText("Drag a rectangle over the ORIGINAL panel.")),
      radioButtons(ns("mode"), "Mode", c("Basic" = "basic", "Advanced" = "advanced"), inline = TRUE),
      conditionalPanel("input.mode == 'basic'", ns = ns,
                       checkboxInput(ns("multiclass_basic"), "Multiclass classification", FALSE)),
      conditionalPanel("input.mode == 'advanced'", ns = ns, advanced_controls(ns),
                       sliderInput(ns("alpha"), "Overlay opacity", 0, 1, 0.45, 0.05)),
      tags$h6("3. Analyze"),
      actionButton(ns("analyze"), "Analyze", class = "btn-primary w-100"),
      tags$hr(),
      tags$h6("4. Export"),
      downloadButton(ns("dl_csv"), "Metrics (CSV)", class = "btn-sm w-100 mb-1"),
      downloadButton(ns("dl_overlay"), "Overlay (PNG)", class = "btn-sm w-100 mb-1"),
      downloadButton(ns("dl_zip"), "All results (ZIP)", class = "btn-sm w-100")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6, 6, 6),
      bslib::card(bslib::card_header("ORIGINAL"),
                  plotOutput(ns("p_original"), height = "300px",
                             brush = brushOpts(ns("brush"), resetOnNew = FALSE))),
      bslib::card(bslib::card_header("LEAF MASK"), plotOutput(ns("p_mask"), height = "300px")),
      bslib::card(bslib::card_header("CLASSIFICATION"), plotOutput(ns("p_class"), height = "300px")),
      bslib::card(bslib::card_header("OVERLAY"), plotOutput(ns("p_overlay"), height = "300px"))
    ),
    uiOutput(ns("summary")),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel("Method report (parameters used)", verbatimTextOutput(ns("report")))
    )
  )
}

mod_single_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    img <- reactiveVal(NULL)
    result <- reactiveVal(NULL)

    load_image <- function(path, name) {
      im <- tryCatch(LeafInjuryR::read_leaf(path), error = function(e) {
        showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(im)) return()
      md <- attr(im, "leaf_metadata"); md$filename <- name
      attr(im, "leaf_metadata") <- md
      img(im); result(NULL)
      session$resetBrush("brush")
    }
    observeEvent(input$file, load_image(input$file$datapath, input$file$name))
    observeEvent(input$example1, load_image(LeafInjuryR::leaf_example_images()[1], "leaf_example_01.jpg"))
    observeEvent(input$example2, load_image(LeafInjuryR::leaf_example_images()[2], "leaf_example_02.jpg"))

    observeEvent(input$analyze, {
      req(img())
      args <- analysis_args(input, input$mode == "advanced")
      crop_points <- NULL
      if (input$crop == "manual") {
        b <- input$brush
        if (is.null(b)) {
          showNotification("Manual crop: drag a rectangle on the ORIGINAL panel first.", type = "warning")
          return()
        }
        crop_points <- list(x = c(b$xmin, b$xmax), y = c(b$ymin, b$ymax))
      }
      withProgress(message = "Analysing leaf...", value = 0.3, {
        res <- tryCatch(
          do.call(LeafInjuryR::analyze_leaf,
                  c(list(image = img(), crop = input$crop, crop_points = crop_points,
                         overlay_alpha = input$alpha %||% 0.45), args)),
          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
        result(res)
      })
    })

    output$p_original <- renderPlot({
      req(img())
      LeafInjuryR::plot_leaf_image(img())
      res <- result()
      if (!is.null(res) && res$crop$method != "none") {
        bb <- res$crop$bbox
        rect(bb["x_min"], bb["y_max"], bb["x_max"], bb["y_min"], border = "#1F78B4", lwd = 2)
      }
    })
    output$p_mask <- renderPlot({ req(result()); LeafInjuryR::plot_leaf_image(result()$leaf_mask) })
    output$p_class <- renderPlot({
      req(result())
      res <- result()
      LeafInjuryR::plot_leaf_image(LeafInjuryR::class_map_to_image(res$class_map, res$parameters$multiclass))
      cols <- LeafInjuryR::leaf_class_colors(res$parameters$multiclass)
      legend("bottomright", legend = names(cols), fill = cols, bg = "white", cex = 0.8)
    })
    output$p_overlay <- renderPlot({ req(result()); LeafInjuryR::plot_leaf_image(result()$overlay) })

    output$summary <- renderUI({
      res <- result()
      if (is.null(res)) return(helpText("Upload an image (or use an example) and press Analyze."))
      m <- res$metrics
      boxes <- bslib::layout_columns(
        bslib::value_box("Leaf area", sprintf("%s px", format(m$leaf_pixels, big.mark = ","))),
        bslib::value_box("Healthy (%)", fmt_pct(m$healthy_percent)),
        bslib::value_box("Injured (%)", fmt_pct(m$injured_percent)),
        bslib::value_box("Quality flag", quality_badge(res$quality$quality_flag))
      )
      extra <- NULL
      if (isTRUE(m$multiclass)) {
        extra <- tags$p(sprintf("Chlorotic: %s | Necrotic: %s | Other: %s",
                                fmt_pct(m$chlorotic_percent), fmt_pct(m$necrotic_percent),
                                fmt_pct(m$other_percent)))
      }
      msgs <- res$quality$quality_messages
      tagList(boxes, extra,
              if (length(msgs)) tags$div(class = "alert alert-warning",
                                         tags$b("Quality messages (results were not modified):"),
                                         tags$ul(lapply(msgs, tags$li))),
              helpText(sprintf("Heuristic diagnostic score: %.2f (not a probability).",
                               res$quality$diagnostic_score)))
    })
    output$report <- renderText({
      req(result()); paste(LeafInjuryR::analysis_report(result()), collapse = "\n")
    })

    stem <- reactive({
      f <- result()$metadata$filename
      if (is.null(f) || is.na(f)) "leaf" else tools::file_path_sans_ext(f)
    })
    output$dl_csv <- downloadHandler(
      filename = function() paste0(stem(), "_metrics.csv"),
      content = function(file) { req(result()); LeafInjuryR::write_leaf_results(result(), file) })
    output$dl_overlay <- downloadHandler(
      filename = function() paste0(stem(), "_overlay.png"),
      content = function(file) { req(result()); LeafInjuryR::write_image_png(result()$overlay, file) })
    output$dl_zip <- downloadHandler(
      filename = function() paste0(stem(), "_results.zip"),
      content = function(file) {
        req(result())
        d <- tempfile("leaf_export_"); dir.create(d)
        files <- LeafInjuryR::export_leaf_analysis(result(), d, prefix = stem())
        if (!safe_zip(file, files)) {
          showNotification("ZIP creation failed (no zip utility found). Download files individually.",
                           type = "error")
        }
      })
  })
}
