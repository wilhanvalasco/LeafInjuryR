# LeafInjuryR - local Shiny interface
#
# Launch with LeafInjuryR::run_leafinjury_app().
# Four areas: Imagem, Segmentacao, Analise, Resultados. All computations call
# the package functions (crop_leaf(), segment_leaf(), analyze_leaf(),
# analyze_leaf_batch(), validate_leaf()) or their internal helpers. Nothing
# leaves the computer: "upload" only reads a local file into this R session.

library(shiny)
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

ui <- bslib::page_navbar(
  id = "main_nav",
  title = tags$span(class = "lir-brand", "LeafInjuryR"),
  theme = lir_theme(),
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "leafinjury.css"),
    tags$script(src = "leafinjury.js")
  ),
  bslib::nav_panel(tagList(ico("image"), "Imagem"), value = "imagem", mod_image_ui("image")),
  bslib::nav_panel(tagList(ico("layers"), "Segmenta\u00e7\u00e3o"), value = "segmentacao", mod_segment_ui("segment")),
  bslib::nav_panel(tagList(ico("play"), "An\u00e1lise"), value = "analise", mod_analysis_ui("analysis")),
  bslib::nav_panel(tagList(ico("chart"), "Resultados"), value = "resultados", mod_results_ui("results")),
  bslib::nav_spacer(),
  bslib::nav_item(tags$span(class = "lir-privacy", ico("shield"), "Processamento local"))
)

server <- function(input, output, session) {
  rv <- new_app_state()
  vh <- reactive(list(h = input$lir_vh %||% 900, fs = isTRUE(input$lir_fs)))
  goto <- function(tab) bslib::nav_select("main_nav", tab, session = session)
  settings <- mod_segment_server("segment", rv, vh)
  mod_image_server("image", rv, vh)
  mod_analysis_server("analysis", rv, vh, settings, goto)
  mod_results_server("results", rv)
}

shinyApp(ui, server)
