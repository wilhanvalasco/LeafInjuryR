# Area 2 - Segmentacao: automatic segmentation, manual correction, mask view

mod_segment_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    class = "lir-layout",
    sidebar = bslib::sidebar(
      width = 300, open = "always",
      section_title("Modo"),
      radioButtons(ns("mode"), NULL, inline = TRUE, opts("Autom\u00e1tica", "auto", "Corre\u00e7\u00e3o manual", "manual")),
      section_title("Visualiza\u00e7\u00e3o"),
      selectInput(ns("view"), NULL, opts("Sobreposi\u00e7\u00e3o", "overlay", "Classifica\u00e7\u00e3o", "classes", "M\u00e1scara foliar", "mask", "Original", "original", "Comparar m\u00e9todos", "methods")),
      checkboxInput(ns("multiclass"), "Separar clorose e necrose", FALSE),
      actionButton(ns("run"), tagList(ico("layers"), "Segmentar"), class = "btn-primary w-100"),
      conditionalPanel(
        "input.mode == 'manual'", ns = ns,
        div(class = "lir-tools",
          section_title("Ferramenta"),
          radioButtons(ns("tool"), NULL,
                       opts("Adicionar \u00e0 folha", "add_leaf", "Remover da folha", "remove_leaf", "Marcar saud\u00e1vel", "healthy", "Marcar injuriado", "injured", "Marcar clor\u00f3tico", "chlorotic", "Marcar necr\u00f3tico", "necrotic")),
          radioButtons(ns("shape"), NULL, inline = TRUE, opts("Pincel (clique)", "circle", "Ret\u00e2ngulo", "rect")),
          conditionalPanel("input.shape == 'circle'", ns = ns,
                           sliderInput(ns("radius"), "Tamanho do pincel (px)", 2, 80, 12, 1)),
          conditionalPanel("input.shape == 'rect'", ns = ns,
                           actionButton(ns("apply_rect"), tagList(ico("check"), "Aplicar na sele\u00e7\u00e3o"),
                                        class = "btn-light w-100 mb-2")),
          div(class = "lir-btn-row",
              actionButton(ns("undo"), tagList(ico("undo"), "Desfazer"), class = "btn-light"),
              actionButton(ns("clear"), tagList(ico("trash"), "Limpar"), class = "btn-light")),
          uiOutput(ns("ncorr")))),
      advanced(
        selectInput(ns("method"), "Separa\u00e7\u00e3o folha/fundo",
                    opts("Autom\u00e1tico (croma + escurid\u00e3o)", "auto", "CIELAB croma", "lab", "HSV satura\u00e7\u00e3o", "hsv", "Excesso de verde (ExG)", "exg", "Otsu (cinza)", "otsu", "Adaptativo (ilumina\u00e7\u00e3o)", "adaptive", "k-means (Lab)", "kmeans")),
        selectInput(ns("tissue_method"), "Classifica\u00e7\u00e3o do tecido",
                    opts("CIELAB (limiares fixos)", "lab", "Relativo \u00e0 cor da pr\u00f3pria folha", "relative", "HSV", "hsv", "ExG \u2212 ExR", "exgr", "Vota\u00e7\u00e3o", "vote", "Auto (Otsu limitado)", "auto")),
        checkboxInput(ns("normalize"), "Normalizar balan\u00e7o de branco", FALSE),
        numericInput(ns("healthy_hue"), "Matiz Lab m\u00ednimo do tecido saud\u00e1vel (\u00b0)", 100, 60, 160, 1),
        numericInput(ns("necrotic_hue"), "Matiz Lab m\u00e1ximo da necrose (\u00b0)", 85, 30, 120, 1),
        numericInput(ns("min_object"), "Tamanho m\u00ednimo de objeto (px; vazio = autom\u00e1tico)", NA, 0),
        numericInput(ns("opening"), "Raio da abertura morfol\u00f3gica (px)", 1, 0, 10, 1),
        selectInput(ns("holes"), "Furos internos",
                    opts("Preencher se n\u00e3o forem fundo", "fill_non_background", "Preencher todos", "fill_all", "N\u00e3o preencher", "none")),
        sliderInput(ns("alpha"), "Opacidade da sobreposi\u00e7\u00e3o", 0.1, 0.9, 0.45, 0.05))
    ),
    conditionalPanel("input.view != 'methods'", ns = ns, viewer_ui(ns, "view")),
    conditionalPanel("input.view == 'methods'", ns = ns,
      div(class = "lir-viewer", div(class = "lir-toolbar",
        div(class = "lir-toolbar-left", tags$small(class = "text-muted",
            "Concord\u00e2ncia entre m\u00e9todos n\u00e3o \u00e9 acur\u00e1cia; nenhum m\u00e9todo \u00e9 declarado o melhor sem refer\u00eancia manual."))),
        div(class = "lir-stage", plotOutput(ns("methods_plot"), height = "640px"))))
  )
}

mod_segment_server <- function(id, rv, vh) {
  moduleServer(id, function(input, output, session) {
    settings <- reactive({
      num <- function(v) if (is.null(v) || is.na(v)) NULL else v
      list(method = input$method, tissue_method = input$tissue_method,
           multiclass = isTRUE(input$multiclass), normalize = isTRUE(input$normalize),
           params = list(
             morphology = list(opening_radius = input$opening %||% 1,
                               min_object_size = num(input$min_object),
                               fill_holes = input$holes != "none",
                               hole_policy = if (input$holes == "none") "fill_non_background" else input$holes),
             tissue = list(lab_healthy_hue_min = input$healthy_hue %||% 100,
                           lab_necrotic_hue_max = min(input$necrotic_hue %||% 85, input$healthy_hue %||% 100))),
           alpha = input$alpha)
    })
    key <- reactive(paste(rv$img_version, rv$crop_version,
                          paste(deparse(settings()[setdiff(names(settings()), "alpha")]), collapse = "")))

    work_arr <- reactive({
      req(rv$seg_auto)
      LeafInjuryR:::as_rgb_array(rv$seg_auto$normalized %||% rv$cropped)
    })

    # Automatic segmentation, recomputed only when image, crop or settings change
    ensure_seg <- function() {
      req(rv$cropped)
      if (!is.null(rv$seg_auto) && identical(rv$seg_key, key())) return(invisible(TRUE))
      s <- settings()
      withProgress(message = "Segmentando...", value = 0.4, {
        seg <- tryCatch(LeafInjuryR::segment_leaf(rv$cropped, "auto", method = s$method,
                          tissue_method = s$tissue_method, multiclass = s$multiclass,
                          normalize = s$normalize, params = s$params),
                        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      })
      req(seg)
      rv$seg_auto <- seg; rv$seg_key <- key(); rv$seg_settings <- s
      rv$seg <- if (nrow(rv$corrections))
        .lir$apply_corrections(seg, work_arr(), rv$corrections, seg$config) else seg
      rv$analysis <- NULL
      invisible(TRUE)
    }
    observeEvent(input$run, ensure_seg())
    observeEvent(input$mode, if (input$mode == "manual" && !is.null(rv$cropped)) ensure_seg(),
                 ignoreInit = TRUE)

    add_correction <- function(row) {
      if (is.null(rv$seg)) ensure_seg()
      req(rv$seg)
      act <- row$action
      if (act %in% c("chlorotic", "necrotic") && !isTRUE(rv$seg$multiclass)) row$action <- "injured"
      rv$seg <- .lir$apply_corrections(rv$seg, work_arr(), row, rv$seg$config)
      rv$corrections <- rbind(rv$corrections, row)
      rv$analysis <- NULL
    }
    view <- viewer_server("view", input, output, session,
      view_fn = reactive({
        req(rv$cropped)
        img <- .lir$view_image(rv$cropped, rv$seg, input$view, alpha = input$alpha)
        .lir$display_raster(img)
      }), vh = vh,
      empty_text = "Confirme uma imagem na \u00e1rea Imagem e clique em Segmentar.")
    observeEvent(view$click(), {
      req(input$mode == "manual", input$shape == "circle")
      e <- view$click()
      add_correction(.lir$new_correction(input$tool, "circle", x = e$x, y = e$y, radius = input$radius))
    })
    observeEvent(input$apply_rect, {
      b <- view$brush()
      if (is.null(b)) { showNotification("Arraste um ret\u00e2ngulo sobre a imagem.", type = "warning"); return() }
      add_correction(.lir$new_correction(input$tool, "rect", xmin = b$xmin, xmax = b$xmax,
                                         ymin = b$ymin, ymax = b$ymax))
    })
    observeEvent(input$undo, {
      req(rv$seg_auto, nrow(rv$corrections) > 0)
      rv$corrections <- rv$corrections[-nrow(rv$corrections), , drop = FALSE]
      rv$seg <- if (nrow(rv$corrections))
        .lir$apply_corrections(rv$seg_auto, work_arr(), rv$corrections, rv$seg_auto$config) else rv$seg_auto
      rv$analysis <- NULL
    })
    observeEvent(input$clear, {
      req(rv$seg_auto)
      rv$corrections <- .lir$as_corrections(NULL); rv$seg <- rv$seg_auto; rv$analysis <- NULL
    })
    output$ncorr <- renderUI(helpText(sprintf("%d corre\u00e7\u00e3o(\u00f5es) registrada(s).", nrow(rv$corrections))))
    output$view_badge <- renderUI({
      req(rv$seg)
      m <- rv$seg$metrics
      tags$span(class = "lir-badge lir-badge-strong",
                tags$b(fmt_pct(m$injured_percent)), " inj\u00faria",
                if (nrow(rv$corrections)) tags$small(sprintf(" \u00b7 %d corre\u00e7\u00f5es", nrow(rv$corrections))))
    })
    output$methods_plot <- renderPlot({
      req(rv$cropped, input$view == "methods")
      withProgress(message = "Comparando m\u00e9todos...", value = 0.5, {
        cmp <- .lir$compare_segmentation_methods(rv$cropped, methods = c("hsv", "lab", "exg", "otsu", "auto"))
      })
      plot(cmp)
    }, res = 96)
    settings
  })
}
