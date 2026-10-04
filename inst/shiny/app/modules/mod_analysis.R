# Area 3 - Analise: individual or batch processing, progress, indicators

mod_analysis_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    class = "lir-layout",
    sidebar = bslib::sidebar(
      width = 300, open = "always",
      section_title("Tipo de an\u00e1lise"),
      radioButtons(ns("scope"), NULL, inline = TRUE, c("Individual" = "single", "Lote" = "batch")),
      conditionalPanel("input.scope == 'single'", ns = ns,
        helpText("Usa a imagem, o recorte, a segmenta\u00e7\u00e3o e as corre\u00e7\u00f5es definidos nas \u00e1reas anteriores."),
        actionButton(ns("run_single"), tagList(ico("play"), "Analisar imagem"), class = "btn-primary w-100")),
      conditionalPanel("input.scope == 'batch'", ns = ns,
        fileInput(ns("files"), NULL, multiple = TRUE, buttonLabel = "Selecionar",
                  placeholder = "Imagens ou ZIP",
                  accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff", ".zip")),
        radioButtons(ns("batch_crop"), "Recorte", inline = TRUE, opts("Autom\u00e1tico", "auto", "Nenhum", "none")),
        actionButton(ns("run_batch"), tagList(ico("play"), "Processar lote"), class = "btn-primary w-100"),
        helpText("Os par\u00e2metros de segmenta\u00e7\u00e3o da \u00e1rea Segmenta\u00e7\u00e3o s\u00e3o aplicados a todas as imagens.")),
      advanced(numericInput(ns("ppc"), "Escala (pixels por cm; vazio = sem escala)", NA, min = 0),
               helpText("\u00c1reas em cm\u00b2 s\u00f3 s\u00e3o calculadas com uma refer\u00eancia de escala v\u00e1lida, fotografada no plano da folha."))
    ),
    conditionalPanel("input.scope == 'single'", ns = ns,
      uiOutput(ns("single_kpis")),
      div(class = "lir-viewtabs",
          radioButtons(ns("single_view"), NULL, inline = TRUE,
                       opts("Comparar", "compare", "Sobreposi\u00e7\u00e3o", "overlay", "Classifica\u00e7\u00e3o", "classes", "M\u00e1scara", "mask", "Original", "original"))),
      conditionalPanel("input.single_view == 'compare'", ns = ns, uiOutput(ns("compare"))),
      conditionalPanel("input.single_view != 'compare'", ns = ns,
                       viewer_ui(ns, "view", click = FALSE, brush = FALSE))),
    conditionalPanel("input.scope == 'batch'", ns = ns,
      uiOutput(ns("batch_kpis")),
      bslib::layout_columns(col_widths = c(5, 7),
        bslib::card(class = "lir-card", bslib::card_header("Imagens processadas"),
                    DT::DTOutput(ns("table"))),
        bslib::card(class = "lir-card", bslib::card_header(uiOutput(ns("preview_title"))),
                    uiOutput(ns("preview")),
                    actionButton(ns("inspect"), tagList(ico("eye"), "Abrir para inspe\u00e7\u00e3o e corre\u00e7\u00e3o"),
                                 class = "btn-light mt-2"))))
  )
}

mod_analysis_server <- function(id, rv, vh, settings, goto) {
  moduleServer(id, function(input, output, session) {
    ppc <- reactive(if (is.null(input$ppc) || is.na(input$ppc) || input$ppc <= 0) NULL else input$ppc)

    observeEvent(input$run_single, {
      if (is.null(rv$cropped)) { showNotification("Carregue uma imagem na \u00e1rea Imagem.", type = "warning"); return() }
      s <- settings()
      key_now <- paste(rv$img_version, rv$crop_version,
                       paste(deparse(s[setdiff(names(s), "alpha")]), collapse = ""))
      seg <- if (!is.null(rv$seg) && identical(rv$seg_key, key_now)) rv$seg else NULL
      if (!is.null(rv$seg) && is.null(seg) && nrow(rv$corrections))
        showNotification("Par\u00e2metros alterados: a segmenta\u00e7\u00e3o foi refeita e as corre\u00e7\u00f5es reaplicadas.", type = "message")
      bb <- rv$crop_info$bbox
      withProgress(message = "Analisando...", value = 0.4, {
        res <- tryCatch({
          r <- LeafInjuryR::analyze_leaf(
            rv$original, crop = if (rv$crop_mode == "none") "none" else "manual",
            crop_points = list(x = bb[c("x_min", "x_max")], y = bb[c("y_min", "y_max")]),
            method = s$method, tissue_method = s$tissue_method, multiclass = s$multiclass,
            normalize = s$normalize, segmentation = seg,
            corrections = if (is.null(seg) && nrow(rv$corrections)) rv$corrections,
            pixels_per_cm = ppc(), params = s$params)
          r$crop$method <- rv$crop_mode; r$parameters$crop_method <- rv$crop_mode
          r$metrics$crop_method <- rv$crop_mode
          if (!is.null(rv$path) && file.exists(rv$path)) {
            r$metrics$file_md5 <- unname(tools::md5sum(rv$path)); r$parameters$file_md5 <- r$metrics$file_md5
          }
          r
        }, error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      })
      req(res)
      if (is.null(seg)) { rv$seg <- res$segmentation; rv$seg_key <- key_now }
      rv$analysis <- res; rv$validation <- NULL; rv$last_result <- "single"
    })

    output$single_kpis <- renderUI({
      res <- rv$analysis
      m <- if (is.null(res)) list() else res$metrics
      div(class = "lir-kpis",
        kpi("Inj\u00faria foliar", fmt_pct(m$injured_percent), "pixels injuriados / pixels da folha", "accent"),
        kpi("\u00c1rea foliar", if (is.null(res)) "\u2014" else paste(fmt_int(m$leaf_pixels), "px"),
            if (!is.null(m$leaf_area_cm2)) sprintf("%.2f cm\u00b2", m$leaf_area_cm2) else "em pixels"),
        kpi("Tecido saud\u00e1vel", fmt_pct(m$healthy_percent),
            if (isTRUE(m$multiclass)) sprintf("clor\u00f3tico %s \u00b7 necr\u00f3tico %s",
                                              fmt_pct(m$chlorotic_percent, 1), fmt_pct(m$necrotic_percent, 1))),
        kpi("Qualidade", flag_label(res$quality$quality_flag),
            if (!is.null(res) && length(res$quality$quality_messages))
              paste(length(res$quality$quality_messages), "alerta(s) \u2014 ver Resultados"),
            paste0("flag-", res$quality$quality_flag %||% "none")))
    })
    output$compare <- renderUI({
      res <- rv$analysis
      if (is.null(res)) return(div(class = "lir-empty", "Clique em Analisar imagem."))
      compare_slider(.lir$image_data_uri(res$cropped, 1600), .lir$image_data_uri(res$overlay, 1600),
                     "Original", sprintf("Processada \u00b7 %s inj\u00faria", fmt_pct(res$metrics$injured_percent)))
    })
    viewer_server("view", input, output, session, vh = vh,
      view_fn = reactive({
        res <- rv$analysis; req(res, input$single_view != "compare")
        seg <- list(class_map = res$class_map, leaf_mask = res$leaf_mask,
                    multiclass = res$parameters$multiclass)
        .lir$display_raster(.lir$view_image(res$cropped, seg, input$single_view))
      }))
    output$view_badge <- renderUI({
      req(rv$analysis)
      tags$span(class = "lir-badge lir-badge-strong", tags$b(fmt_pct(rv$analysis$metrics$injured_percent)), " inj\u00faria")
    })

    # ----- batch -----
    counts <- reactiveVal(c(n = 0, done = 0, failed = 0))
    observeEvent(input$files, {
      rv$batch_files <- stage_uploads(input$files$datapath, input$files$name)
      counts(c(n = length(rv$batch_files), done = 0, failed = 0))
      if (!length(rv$batch_files)) showNotification("Nenhuma imagem compat\u00edvel encontrada.", type = "error")
    })
    observeEvent(input$run_batch, {
      files <- rv$batch_files
      if (!length(files)) { showNotification("Selecione imagens ou um arquivo ZIP.", type = "warning"); return() }
      s <- settings(); n <- length(files); failed <- 0
      out_dir <- tempfile("lir_batch_")
      withProgress(message = "Processando lote", value = 0, {
        cb <- function(i, n, file, status) {
          if (!identical(status, "ok")) failed <<- failed + 1
          counts(c(n = n, done = i, failed = failed))
          incProgress(1 / n, detail = sprintf("%d de %d \u00b7 restantes %d \u00b7 com erro %d", i, n, n - i, failed))
        }
        b <- tryCatch(LeafInjuryR::analyze_leaf_batch(
          files, output_dir = out_dir, progress = FALSE, callback = cb, crop = input$batch_crop,
          method = s$method, tissue_method = s$tissue_method, multiclass = s$multiclass,
          normalize = s$normalize, pixels_per_cm = ppc(), params = s$params),
          error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      })
      req(b)
      rv$batch <- b; rv$batch_dir <- out_dir; rv$last_result <- "batch"
      showNotification(sprintf("Lote conclu\u00eddo: %d imagens, %d com erro. Relat\u00f3rios gerados.",
                               n, sum(b$results$status != "ok")), type = "message")
    })
    output$batch_kpis <- renderUI({
      k <- counts(); b <- rv$batch
      v <- if (!is.null(b)) b$results$injured_percent[b$results$status == "ok"] else numeric(0)
      div(class = "lir-kpis",
        kpi("Imagens", k[["n"]]), kpi("Processadas", k[["done"]]),
        kpi("Restantes", k[["n"]] - k[["done"]]),
        kpi("Com erro", k[["failed"]], NULL, if (k[["failed"]] > 0) "flag-failed" else ""),
        kpi("Inj\u00faria m\u00e9dia", if (length(v)) fmt_pct(mean(v)) else "\u2014", NULL, "accent"))
    })
    table_df <- reactive({
      b <- rv$batch; req(b); r <- b$results
      stats::setNames(data.frame(r$file, round(r$injured_percent, 2),
                                 vapply(r$quality_flag, flag_label, ""),
                                 ifelse(r$status == "ok", "conclu\u00edda", "erro"), stringsAsFactors = FALSE),
                      c("Imagem", "Inj\u00faria (%)", "Qualidade", "Status"))
    })
    output$table <- DT::renderDT(DT::datatable(
      table_df(), selection = list(mode = "single", selected = 1), rownames = FALSE,
      options = list(pageLength = 12, dom = "ftip", scrollX = TRUE,
                     language = dt_pt)))
    selected <- reactive({ b <- rv$batch; i <- input$table_rows_selected; req(b, length(i) == 1); i })
    output$preview_title <- renderUI({
      req(rv$batch); i <- selected(); r <- rv$batch$results[i, ]
      tags$span(r$file, tags$small(class = "text-muted ms-2", fmt_pct(r$injured_percent)))
    })
    output$preview <- renderUI({
      b <- rv$batch; i <- selected(); r <- b$results[i, ]
      if (r$status != "ok") return(div(class = "lir-empty error", r$error_message))
      th <- b$thumbnails[[r$output_stem]]
      compare_slider(th$original, th$processed, "Original", "Processada")
    })
    observeEvent(input$inspect, {
      req(rv$batch); i <- selected()
      f <- rv$batch_files[i]
      im <- tryCatch(LeafInjuryR:::read_leaf(f), error = function(e) NULL); req(im)
      md <- attr(im, "leaf_metadata"); md$filename <- basename(f); attr(im, "leaf_metadata") <- md
      rv$original <- im; rv$name <- basename(f); rv$path <- f; rv$img_version <- rv$img_version + 1
      rv$cropped <- im; rv$crop_mode <- "none"; rv$crop_version <- rv$crop_version + 1
      d <- dim(im)
      rv$crop_info <- list(method = "none", found = NA, original_dim = d[1:2],
                           bbox = c(x_min = 1, x_max = d[1], y_min = 1, y_max = d[2]))
      rv$seg_auto <- NULL; rv$seg <- NULL; rv$corrections <- .lir$as_corrections(NULL); rv$analysis <- NULL
      goto("imagem")
    })
  })
}
