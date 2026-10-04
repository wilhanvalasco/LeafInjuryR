# Area 1 - Imagem: import, large view, automatic/manual crop, framing

mod_image_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    class = "lir-layout",
    sidebar = bslib::sidebar(
      width = 300, open = "always",
      section_title("Fotografia"),
      fileInput(ns("file"), NULL, accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff"),
                buttonLabel = "Selecionar", placeholder = "JPG, PNG ou TIFF"),
      div(class = "lir-examples", "Exemplos:",
          actionLink(ns("ex1"), "folha 1"), "\u00b7", actionLink(ns("ex2"), "folha 2")),
      section_title("Recorte"),
      radioButtons(ns("crop_mode"), NULL, inline = TRUE,
                   opts("Autom\u00e1tico", "auto", "Manual", "manual", "Nenhum", "none")),
      uiOutput(ns("hint")),
      div(class = "lir-btn-row",
          actionButton(ns("apply"), tagList(ico("check"), "Confirmar"), class = "btn-primary"),
          actionButton(ns("reset"), tagList(ico("refresh"), "Redefinir"), class = "btn-light")),
      advanced(sliderInput(ns("margin"), "Margem do recorte autom\u00e1tico", 0, 0.2, 0.05, 0.01)),
      uiOutput(ns("info"))
    ),
    viewer_ui(ns, "view", toolbar_left = radioButtons(
      ns("show"), NULL, inline = TRUE, c("Original" = "original", "Recortada" = "cropped")))
  )
}

mod_image_server <- function(id, rv, vh) {
  moduleServer(id, function(input, output, session) {
    load_image <- function(path, name) {
      im <- tryCatch(LeafInjuryR:::read_leaf(path), error = function(e) {
        showNotification(conditionMessage(e), type = "error"); NULL })
      if (is.null(im)) return()
      md <- attr(im, "leaf_metadata"); md$filename <- name; attr(im, "leaf_metadata") <- md
      rv$original <- im; rv$name <- name; rv$path <- path
      rv$img_version <- rv$img_version + 1
      set_crop(im, "none", NULL)
      updateRadioButtons(session, "show", selected = "original")
    }
    set_crop <- function(img, mode, info) {
      rv$cropped <- img; rv$crop_mode <- mode
      d <- dim(rv$original)
      rv$crop_info <- info %||% list(method = "none", found = NA, original_dim = d[1:2],
                                     bbox = c(x_min = 1, x_max = d[1], y_min = 1, y_max = d[2]))
      rv$crop_version <- rv$crop_version + 1
      rv$seg_auto <- NULL; rv$seg <- NULL; rv$seg_key <- NULL
      rv$corrections <- .lir$as_corrections(NULL); rv$analysis <- NULL; rv$validation <- NULL
    }
    observeEvent(input$file, load_image(input$file$datapath, input$file$name))
    ex <- function(k) system.file("extdata", sprintf("leaf_example_%02d.jpg", k), package = "LeafInjuryR")
    observeEvent(input$ex1, load_image(ex(1), "leaf_example_01.jpg"))
    observeEvent(input$ex2, load_image(ex(2), "leaf_example_02.jpg"))

    # automatic crop preview (cached per image and margin)
    auto_preview <- reactive({
      req(rv$original, input$crop_mode == "auto")
      cr <- LeafInjuryR::crop_leaf(rv$original, "auto", margin = input$margin)
      attr(cr, "crop_info")
    })
    output$hint <- renderUI({
      txt <- switch(input$crop_mode,
        auto = "A regi\u00e3o da folha \u00e9 detectada com margem de seguran\u00e7a. Confira o ret\u00e2ngulo e confirme.",
        manual = "Arraste sobre a imagem para selecionar a regi\u00e3o e clique em Confirmar.",
        none = "A imagem inteira ser\u00e1 analisada.")
      helpText(txt)
    })
    observeEvent(input$apply, {
      req(rv$original)
      if (input$crop_mode == "auto") {
        cr <- LeafInjuryR::crop_leaf(rv$original, "auto", margin = input$margin)
        if (!isTRUE(attr(cr, "crop_info")$found))
          showNotification("Nenhuma folha detectada; mantida a imagem inteira.", type = "warning")
        set_crop(cr, "auto", attr(cr, "crop_info"))
      } else if (input$crop_mode == "manual") {
        b <- view$brush()
        if (is.null(b)) {
          showNotification("Arraste um ret\u00e2ngulo sobre a imagem antes de confirmar.", type = "warning")
          return()
        }
        cr <- tryCatch(LeafInjuryR::crop_leaf(rv$original, "manual", x = c(b$xmin, b$xmax),
                                              y = c(b$ymin, b$ymax)),
                       error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
        req(cr); set_crop(cr, "manual", attr(cr, "crop_info"))
      } else {
        set_crop(rv$original, "none", NULL)
      }
      updateRadioButtons(session, "show", selected = "cropped")
      showNotification("Recorte confirmado.", type = "message", duration = 2)
    })
    observeEvent(input$reset, {
      req(rv$original); set_crop(rv$original, "none", NULL)
      updateRadioButtons(session, "show", selected = "original")
    })

    raster <- reactive({
      req(rv$original)
      .lir$display_raster(if (input$show == "cropped") rv$cropped else rv$original)
    })
    view <- viewer_server("view", input, output, session,
      view_fn = reactive(if (is.null(rv$original)) NULL else raster()), vh = vh,
      extra_fn = function() {
        if (input$show != "original") return()
        if (input$crop_mode == "auto") {
          info <- tryCatch(auto_preview(), error = function(e) NULL)
          if (!is.null(info) && isTRUE(info$found)) {
            bb <- info$bbox
            rect(bb[["x_min"]], bb[["y_max"]], bb[["x_max"]], bb[["y_min"]], border = "#3E8E5E", lwd = 2, lty = 2)
          }
        }
        if (rv$crop_mode != "none") {
          bb <- rv$crop_info$bbox
          rect(bb[["x_min"]], bb[["y_max"]], bb[["x_max"]], bb[["y_min"]], border = "#1F2A24", lwd = 1.5)
        }
      })
    output$view_badge <- renderUI({
      req(rv$original)
      d <- dim(if (input$show == "cropped") rv$cropped else rv$original)
      tags$span(class = "lir-badge", sprintf("%d \u00d7 %d px", d[1], d[2]))
    })
    output$info <- renderUI({
      req(rv$original)
      d <- dim(rv$original); bb <- rv$crop_info$bbox
      div(class = "lir-info",
        div(tags$span("Arquivo"), tags$b(rv$name)),
        div(tags$span("Dimens\u00f5es"), tags$b(sprintf("%d \u00d7 %d px", d[1], d[2]))),
        div(tags$span("Recorte"), tags$b(switch(rv$crop_mode, none = "nenhum",
          sprintf("%s \u00b7 %d \u00d7 %d px", rv$crop_mode, bb[["x_max"]] - bb[["x_min"]] + 1,
                  bb[["y_max"]] - bb[["y_min"]] + 1)))))
    })
  })
}
