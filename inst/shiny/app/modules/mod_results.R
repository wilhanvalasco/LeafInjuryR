# Area 4 - Resultados: quantitative results, comparison, validation, export

mod_results_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    class = "lir-layout",
    sidebar = bslib::sidebar(
      width = 300, open = "always",
      section_title("Resultados de"),
      radioButtons(ns("source"), NULL, inline = TRUE, opts("An\u00e1lise individual", "single", "Lote", "batch")),
      section_title("Exportar"),
      div(class = "lir-export",
        downloadButton(ns("dl_html"), tagList(ico("file"), "Relat\u00f3rio HTML"), icon = NULL, class = "btn-light"),
        downloadButton(ns("dl_xlsx"), tagList(ico("file"), "Planilha Excel"), icon = NULL, class = "btn-light"),
        downloadButton(ns("dl_csv"), tagList(ico("file"), "Base CSV"), icon = NULL, class = "btn-light"),
        downloadButton(ns("dl_zip"), tagList(ico("download"), "Tudo (ZIP)"), icon = NULL, class = "btn-primary")),
      section_title("Valida\u00e7\u00e3o"),
      helpText("Opcional: m\u00e1scaras manuais de refer\u00eancia (PNG, branco = objeto) no tamanho da fotografia original."),
      fileInput(ns("ref_leaf"), "M\u00e1scara da folha", accept = c(".png", ".tif", ".tiff"), buttonLabel = "Selecionar", placeholder = "Nenhum arquivo"),
      fileInput(ns("ref_injury"), "M\u00e1scara da inj\u00faria", accept = c(".png", ".tif", ".tiff"), buttonLabel = "Selecionar", placeholder = "Nenhum arquivo"),
      actionButton(ns("validate"), tagList(ico("shield"), "Validar"), class = "btn-light w-100")
    ),
    uiOutput(ns("kpis")),
    bslib::layout_columns(col_widths = c(7, 5),
      bslib::card(class = "lir-card", bslib::card_header("Compara\u00e7\u00e3o entre imagens"),
                  plotOutput(ns("chart"), height = "340px")),
      bslib::card(class = "lir-card", bslib::card_header("Valida\u00e7\u00e3o e controle de qualidade"),
                  uiOutput(ns("validation")))),
    conditionalPanel("output.has_validation_plot", ns = ns,
      bslib::card(class = "lir-card", bslib::card_header("M\u00e1scara autom\u00e1tica \u00d7 refer\u00eancia"),
                  plotOutput(ns("validation_plot"), height = "420px"))),
    bslib::card(class = "lir-card", bslib::card_header("Resultados quantitativos"), DT::DTOutput(ns("table")))
  )
}

mod_results_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observeEvent(rv$last_result, updateRadioButtons(session, "source", selected = rv$last_result))
    src <- reactive({
      if (input$source == "single") rv$analysis else rv$batch
    })
    report <- reactive({ x <- src(); req(x); .lir$as_report_source(x) })

    output$kpis <- renderUI({
      x <- src()
      if (is.null(x)) return(div(class = "lir-empty", "Nenhum resultado ainda. Execute uma an\u00e1lise na \u00e1rea An\u00e1lise."))
      r <- report()$results; ok <- r$status == "ok"; v <- r$injured_percent[ok]
      div(class = "lir-kpis",
        kpi("Imagens", nrow(r), sprintf("%d conclu\u00eddas \u00b7 %d com erro", sum(ok), sum(!ok))),
        kpi("Inj\u00faria m\u00e9dia", if (length(v)) fmt_pct(mean(v)) else "\u2014", NULL, "accent"),
        kpi("Mediana", if (length(v)) fmt_pct(stats::median(v)) else "\u2014"),
        kpi("M\u00ednimo \u2013 m\u00e1ximo", if (length(v)) sprintf("%s \u2013 %s", fmt_pct(min(v), 1), fmt_pct(max(v), 1)) else "\u2014",
            if (length(v) > 1) sprintf("DP %s", fmt_pct(stats::sd(v))) else NULL))
    })
    output$chart <- renderPlot({
      r <- report()$results; ok <- r$status == "ok" & is.finite(r$injured_percent)
      validate(need(any(ok), "Sem an\u00e1lises conclu\u00eddas."))
      r <- r[ok, ]; o <- order(r$injured_percent); r <- r[o, ]
      cols <- ifelse(r$quality_flag == "ok", "#4C9A6A", "#D9A441")
      par(mar = c(4, max(6, min(16, max(nchar(r$file)) * 0.5)), 1, 2), family = "sans",
          col.axis = "#4A554F", fg = "#C9CFCB")
      if (isTRUE(r$multiclass[1]) && nrow(r) == 1) {
        vals <- stats::setNames(c(r$healthy_percent, r$chlorotic_percent, r$necrotic_percent, r$other_percent),
                                c("Saud\u00e1vel", "Clor\u00f3tico", "Necr\u00f3tico", "Outro"))
        barplot(rev(vals), horiz = TRUE, las = 1, border = NA, xlim = c(0, 100),
                col = rev(c("#1FA83A", "#FFD500", "#E31A1C", "#C21BFF")), xlab = "% da \u00e1rea foliar")
      } else {
        bp <- barplot(r$injured_percent, names.arg = r$file, horiz = TRUE, las = 1, border = NA, col = cols,
                      xlim = c(0, max(5, max(r$injured_percent) * 1.15)), xlab = "Inj\u00faria foliar (%)", cex.names = 0.8)
        text(r$injured_percent, bp, sprintf("%.1f", r$injured_percent), pos = 4, cex = 0.75, col = "#4A554F")
        if (nrow(r) > 1) abline(v = mean(r$injured_percent), lty = 2, col = "#1F2A24")
      }
    }, res = 96)
    output$table <- DT::renderDT({
      r <- report()$results
      show <- intersect(c("file", "status", "leaf_pixels", "healthy_pixels", "injured_pixels", "healthy_percent",
                          "injured_percent", "chlorotic_percent", "necrotic_percent", "other_percent",
                          "leaf_area_cm2", "injured_area_cm2", "quality_flag", "manual_corrections",
                          "method", "tissue_method", "error_message"), names(r))
      d <- r[show]
      num <- names(d)[vapply(d, is.numeric, logical(1)) & grepl("percent|cm2", names(d))]
      DT::formatRound(DT::datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "ftip", language = dt_pt)),
                      num, 2)
    })

    observeEvent(input$validate, {
      res <- rv$analysis
      if (is.null(res)) { showNotification("A valida\u00e7\u00e3o com refer\u00eancia \u00e9 feita sobre a an\u00e1lise individual.", type = "warning"); return() }
      v <- tryCatch(LeafInjuryR::validate_leaf(res, input$ref_leaf$datapath, input$ref_injury$datapath),
                    error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
      rv$validation <- v
    })
    output$validation <- renderUI({
      x <- src(); req(x)
      v <- if (!is.null(rv$validation) && input$source == "single") rv$validation else LeafInjuryR::validate_leaf(x)
      q <- v$quality
      qc <- if (is.data.frame(q)) {
        tab <- table(factor(q$quality_flag, c("ok", "warning", "failed")))
        tags$p(sprintf("Qualidade: %d OK \u00b7 %d com aten\u00e7\u00e3o \u00b7 %d com falha", tab[1], tab[2], tab[3]))
      } else tagList(tags$p(tags$b("Qualidade: "), flag_label(q$quality_flag),
                            tags$small(class = "text-muted", sprintf(" \u00b7 indicador heur\u00edstico %.2f (n\u00e3o \u00e9 probabilidade)", q$diagnostic_score))),
                     if (length(q$quality_messages)) tags$ul(class = "lir-msgs", lapply(q$quality_messages, tags$li)))
      met <- if (!is.null(v$metrics)) {
        m <- v$metrics
        rows <- lapply(seq_len(nrow(m)), function(i) tags$tr(
          tags$td(if (m$label[i] == "leaf") "Folha \u00d7 fundo" else "Saud\u00e1vel \u00d7 injuriado"),
          lapply(c("iou", "dice", "sensitivity", "specificity", "precision", "f1"),
                 function(cc) tags$td(sprintf("%.3f", m[[cc]][i])))))
        tagList(tags$table(class = "table table-sm lir-metrics",
          tags$thead(tags$tr(tags$th(""), tags$th("IoU"), tags$th("Dice"), tags$th("Sens."),
                             tags$th("Espec."), tags$th("Prec."), tags$th("F1"))), tags$tbody(rows)),
          if (!is.null(m$injured_percent_error) && any(!is.na(m$injured_percent_error)))
            tags$p(sprintf("Erro do percentual de inj\u00faria: %+.2f pontos percentuais",
                           stats::na.omit(m$injured_percent_error)[1])))
      } else tags$p(class = "text-muted", "Sem m\u00e1scara de refer\u00eancia: s\u00e3o mostrados apenas indicadores de qualidade, sem m\u00e9tricas de acur\u00e1cia.")
      tagList(qc, met)
    })
    output$has_validation_plot <- reactive(input$source == "single" && !is.null(rv$validation) && isTRUE(rv$validation$has_reference))
    outputOptions(output, "has_validation_plot", suspendWhenHidden = FALSE)
    output$validation_plot <- renderPlot({ req(rv$validation$has_reference); plot(rv$validation) }, res = 96)

    # ----- exports: the four outputs use the same results table -----
    report_dir <- reactive({
      x <- src(); req(x)
      if (inherits(x, "leaf_batch") && length(x$report_files)) return(x$output_dir)
      d <- tempfile("lir_report_"); dir.create(d)
      .lir$write_leaf_reports(x, d)
      if (inherits(x, "leaf_analysis")) {
        dir.create(file.path(d, "masks")); dir.create(file.path(d, "overlays"))
        stem <- tools::file_path_sans_ext(x$metrics$file %||% "leaf")
        .lir$save_analysis_images(x, d, if (is.na(stem)) "leaf" else stem)
      }
      d
    })
    stamp <- function() format(Sys.time(), "%Y%m%d_%H%M")
    output$dl_html <- downloadHandler(function() paste0("leafinjury_report_", stamp(), ".html"),
                                      function(file) file.copy(file.path(report_dir(), "report.html"), file))
    output$dl_xlsx <- downloadHandler(function() paste0("leafinjury_results_", stamp(), ".xlsx"),
                                      function(file) file.copy(file.path(report_dir(), "results.xlsx"), file))
    output$dl_csv <- downloadHandler(function() paste0("leafinjury_results_", stamp(), ".csv"),
                                     function(file) file.copy(file.path(report_dir(), "results.csv"), file))
    output$dl_zip <- downloadHandler(function() paste0("leafinjury_", stamp(), ".zip"),
      function(file) {
        if (!isTRUE(tryCatch(zip_dir(file, report_dir()), error = function(e) FALSE)))
          showNotification("N\u00e3o foi poss\u00edvel criar o ZIP (utilit\u00e1rio 'zip' ausente). Baixe os arquivos individualmente.",
                           type = "error")
      })
  })
}
