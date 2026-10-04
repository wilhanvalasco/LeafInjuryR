# Shared UI pieces and application state (presentation only; no analysis logic)

.lir <- asNamespace("LeafInjuryR")
`%||%` <- function(a, b) if (is.null(a)) b else a

# Named choices built with setNames(): labels written as argument names would
# be converted to the native encoding by the R parser (breaks accents outside
# UTF-8 locales); setNames() keeps them in UTF-8 on every system.
opts <- function(...) {
  x <- c(...)
  stats::setNames(x[c(FALSE, TRUE)], x[c(TRUE, FALSE)])
}

lir_theme <- function() {
  bslib::bs_theme(
    version = 5, bg = "#FFFFFF", fg = "#1F2A24", primary = "#3E8E5E", secondary = "#6B756F",
    success = "#3E8E5E", warning = "#C98A1B", danger = "#C2410C",
    base_font = bslib::font_collection("-apple-system", "BlinkMacSystemFont", "Segoe UI",
                                       "Inter", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    "border-radius" = "0.6rem", "btn-border-radius" = "0.55rem", "font-size-base" = "0.9rem"
  )
}

# Linear (stroke) icons, inline SVG: no external icon fonts or CDNs
ico <- function(name) {
  p <- switch(name,
    image = '<rect x="3" y="3" width="18" height="18" rx="2"/><circle cx="9" cy="9" r="2"/><path d="m21 15-3.1-3.1a2 2 0 0 0-2.8 0L6 21"/>',
    layers = '<path d="m12 2 10 5-10 5L2 7z"/><path d="m2 17 10 5 10-5"/><path d="m2 12 10 5 10-5"/>',
    play = '<path d="M6 3l14 9-14 9z"/>',
    chart = '<path d="M3 3v18h18"/><path d="M8 17v-5"/><path d="M13 17V7"/><path d="M18 17v-9"/>',
    crop = '<path d="M6 2v14a2 2 0 0 0 2 2h14"/><path d="M18 22V8a2 2 0 0 0-2-2H2"/>',
    zoomin = '<circle cx="11" cy="11" r="7"/><path d="m21 21-4.3-4.3"/><path d="M11 8v6M8 11h6"/>',
    zoomout = '<circle cx="11" cy="11" r="7"/><path d="m21 21-4.3-4.3"/><path d="M8 11h6"/>',
    fit = '<rect x="4" y="5" width="16" height="14" rx="2"/><path d="M9 12h6"/><path d="m9 12 2-2M9 12l2 2M15 12l-2-2M15 12l-2 2"/>',
    full = '<path d="M15 3h6v6"/><path d="M9 21H3v-6"/><path d="m21 3-7 7"/><path d="m3 21 7-7"/>',
    grid = '<rect x="3" y="3" width="18" height="18" rx="2"/><path d="M3 9h18M3 15h18M9 3v18M15 3v18"/>',
    undo = '<path d="M3 7v6h6"/><path d="M21 17a9 9 0 0 0-15-6.7L3 13"/>',
    trash = '<path d="M3 6h18"/><path d="M19 6l-1 14a2 2 0 0 1-2 2H8a2 2 0 0 1-2-2L5 6"/><path d="M10 11v6M14 11v6"/>',
    download = '<path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><path d="m7 10 5 5 5-5"/><path d="M12 15V3"/>',
    check = '<path d="M20 6 9 17l-5-5"/>',
    refresh = '<path d="M21 12a9 9 0 1 1-3-6.7L21 8"/><path d="M21 3v5h-5"/>',
    brush = '<path d="m9.1 11.9 8-8.1a2.9 2.9 0 1 1 4.1 4.1l-8.1 8"/><path d="M7.1 14.9c-1.7 0-3 1.4-3 3 0 1.3-2.5 1.5-2 2 1.1 1.1 2.5 2 4 2 2.2 0 4-1.8 4-4a3 3 0 0 0-3-3z"/>',
    shield = '<path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"/><path d="m9 12 2 2 4-4"/>',
    file = '<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><path d="M14 2v6h6"/>',
    eye = '<path d="M2 12s3.6-7 10-7 10 7 10 7-3.6 7-10 7S2 12 2 12z"/><circle cx="12" cy="12" r="3"/>',
    '<circle cx="12" cy="12" r="9"/>')
  HTML(sprintf('<svg class="lir-ico" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.75" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>', p))
}

new_app_state <- function() {
  reactiveValues(
    original = NULL, name = NULL, path = NULL, img_version = 0,
    cropped = NULL, crop_info = NULL, crop_mode = "none", crop_version = 0,
    seg_auto = NULL, seg = NULL, seg_key = NULL, seg_settings = NULL,
    corrections = .lir$as_corrections(NULL),
    analysis = NULL, batch = NULL, batch_dir = NULL, batch_files = NULL,
    last_result = NULL, validation = NULL)
}

kpi <- function(label, value, sub = NULL, cls = "") {
  div(class = paste("lir-kpi", cls), tags$span(class = "lir-kpi-label", label),
      tags$b(value), if (!is.null(sub)) tags$small(sub))
}

fmt_pct <- function(x, d = 2) if (is.null(x) || length(x) == 0 || is.na(x)) "\u2014" else
  paste0(formatC(x, format = "f", digits = d, decimal.mark = ","), "%")
fmt_int <- function(x) if (is.null(x) || is.na(x)) "\u2014" else format(round(x), big.mark = ".", decimal.mark = ",")
flag_label <- function(f) switch(f %||% "", ok = "OK", warning = "Aten\u00e7\u00e3o", failed = "Falha", "\u2014")

section_title <- function(text) tags$div(class = "lir-section", text)

advanced <- function(...) {
  bslib::accordion(class = "lir-advanced", open = FALSE,
                   bslib::accordion_panel("Ajustes avan\u00e7ados", ...))
}

# ---------------------------------------------------------------------------
# Large image viewer: plot coordinates = original pixel coordinates.
# Zoom (buttons or double-click), fit to window, optional grid, full screen.
# Height follows the image aspect ratio, limited by the browser height.

viewer_ui <- function(ns, id, toolbar_left = NULL, badge = TRUE, click = TRUE, brush = TRUE) {
  wrap <- ns(paste0(id, "_wrap"))
  div(class = "lir-viewer", id = wrap,
    div(class = "lir-toolbar",
      div(class = "lir-toolbar-left", toolbar_left),
      div(class = "lir-toolbar-right",
        actionButton(ns(paste0(id, "_zin")), ico("zoomin"), class = "lir-icon-btn", title = "Ampliar"),
        actionButton(ns(paste0(id, "_zout")), ico("zoomout"), class = "lir-icon-btn", title = "Reduzir"),
        actionButton(ns(paste0(id, "_fit")), ico("fit"), class = "lir-icon-btn", title = "Ajustar \u00e0 janela"),
        actionButton(ns(paste0(id, "_grid")), ico("grid"), class = "lir-icon-btn", title = "Grade"),
        tags$button(type = "button", class = "btn lir-icon-btn", title = "Tela cheia",
                    onclick = sprintf("lirFullscreen('%s')", wrap), ico("full")))),
    div(class = "lir-stage",
      plotOutput(ns(id), height = "auto",
                 click = if (click) clickOpts(ns(paste0(id, "_click"))),
                 dblclick = dblclickOpts(ns(paste0(id, "_dbl"))),
                 brush = if (brush) brushOpts(ns(paste0(id, "_brush")), fill = "#3E8E5E",
                                              stroke = "#2C6B45", opacity = 0.18,
                                              resetOnNew = TRUE)),
      if (badge) uiOutput(ns(paste0(id, "_badge")), class = "lir-badge-wrap")))
}

# view_fn: reactive returning a display raster (LeafInjuryR:::display_raster) or NULL
# extra_fn: optional function drawing on top (e.g. crop rectangle)
viewer_server <- function(id, input, output, session, view_fn, vh, extra_fn = NULL,
                          empty_text = "Carregue uma fotografia na \u00e1rea Imagem.") {
  st <- reactiveValues(xlim = NULL, ylim = NULL, grid = FALSE)
  dims <- reactive({ v <- view_fn(); if (is.null(v)) NULL else v$dims })
  observeEvent(dims(), { st$xlim <- NULL; st$ylim <- NULL }, ignoreNULL = FALSE)
  zoom <- function(factor, cx = NULL, cy = NULL) {
    d <- dims(); req(d)
    xl <- st$xlim %||% c(0.5, d[1] + 0.5); yl <- st$ylim %||% c(0.5, d[2] + 0.5)
    hw <- diff(xl) / 2 / factor; hh <- diff(yl) / 2 / factor
    if (hw >= d[1] / 2 && hh >= d[2] / 2) { st$xlim <- NULL; st$ylim <- NULL; return() }
    w <- .lir$clamp_window(cx %||% mean(xl), cy %||% mean(yl), hw, hh, d)
    st$xlim <- w$xlim; st$ylim <- w$ylim
  }
  observeEvent(input[[paste0(id, "_zin")]], zoom(2))
  observeEvent(input[[paste0(id, "_zout")]], zoom(0.5))
  observeEvent(input[[paste0(id, "_fit")]], { st$xlim <- NULL; st$ylim <- NULL })
  observeEvent(input[[paste0(id, "_grid")]], st$grid <- !st$grid)
  observeEvent(input[[paste0(id, "_dbl")]], {
    e <- input[[paste0(id, "_dbl")]]; zoom(2, e$x, e$y)
  })
  plot_height <- function() {
    w <- session$clientData[[paste0("output_", session$ns(id), "_width")]] %||% 900
    d <- dims()
    v <- vh()
    maxh <- if (isTRUE(v$fs)) v$h - 70 else max(380, v$h - 205)
    if (is.null(d)) return(min(420, maxh))
    xl <- st$xlim %||% c(0, d[1]); yl <- st$ylim %||% c(0, d[2])
    max(280, min(round(w * diff(yl) / diff(xl)), maxh))
  }
  output[[id]] <- renderPlot({
    v <- view_fn()
    if (is.null(v)) {
      par(mar = c(0, 0, 0, 0)); plot.new()
      text(0.5, 0.5, empty_text, col = "#8A948E", cex = 1.05)
      return(invisible())
    }
    .lir$draw_leaf_view(v, st$xlim, st$ylim, grid = st$grid,
                        bg = if (isTRUE(vh()$fs)) "#111111" else "#FFFFFF")
    if (is.function(extra_fn)) extra_fn()
  }, height = plot_height, res = 96)
  list(click = reactive(input[[paste0(id, "_click")]]),
       brush = reactive(input[[paste0(id, "_brush")]]),
       reset_zoom = function() { st$xlim <- NULL; st$ylim <- NULL })
}

# Original x processed comparison slider (pure HTML/CSS)
compare_slider <- function(left_uri, right_uri, left_label = "Original", right_label = "Processada",
                           id = paste0("cmp", sample.int(1e6, 1))) {
  div(class = "lir-compare-wrap", id = id,
    div(class = "lir-compare", style = "--pos:50%;",
      tags$img(src = right_uri, class = "lir-cmp-base", alt = right_label),
      tags$img(src = left_uri, class = "lir-cmp-top", alt = left_label),
      div(class = "lir-cmp-line"),
      tags$span(class = "lir-cmp-tag left", left_label),
      tags$span(class = "lir-cmp-tag right", right_label),
      tags$input(type = "range", min = 0, max = 100, value = 50, class = "lir-cmp-range",
                 oninput = "this.parentNode.style.setProperty('--pos', this.value + '%')",
                 `aria-label` = "Comparar")),
    tags$button(type = "button", class = "btn lir-icon-btn lir-cmp-full", title = "Tela cheia",
                onclick = sprintf("lirFullscreen('%s')", id), ico("full")))
}

# Read uploaded images (or ZIP archives) into a staging folder, keeping names
stage_uploads <- function(paths, names) {
  d <- tempfile("lir_upload_"); dir.create(d)
  for (i in seq_along(paths)) {
    if (grepl("\\.zip$", names[i], ignore.case = TRUE)) utils::unzip(paths[i], exdir = d)
    else file.copy(paths[i], file.path(d, names[i]))
  }
  .lir$list_leaf_images(d, recursive = TRUE)
}

# Zip a directory keeping its sub-folders
zip_dir <- function(zipfile, dir) {
  old <- setwd(dir); on.exit(setwd(old))
  utils::zip(zipfile, files = list.files(".", recursive = TRUE), flags = "-q")
  file.exists(zipfile)
}

dt_pt <- list(search = "Buscar:", info = "_START_\u2013_END_ de _TOTAL_", infoEmpty = "Nenhum registro",
              zeroRecords = "Nenhum registro", lengthMenu = "Mostrar _MENU_",
              paginate = list(previous = "\u2039", `next` = "\u203a"))
