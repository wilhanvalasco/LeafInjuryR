# LeafInjuryR - local Shiny interface
#
# Launch with LeafInjuryR::run_leafinjury_app().
# All computations call the same functions as the R API (analyze_leaf(),
# analyze_leaf_batch(), compare_segmentation_methods(), ...). Nothing is sent
# to external servers: files are read into the local R session only.

library(shiny)

for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

ui <- bslib::page_navbar(
  title = "LeafInjuryR",
  theme = bslib::bs_theme(version = 5),
  bslib::nav_panel("Single analysis", mod_single_ui("single")),
  bslib::nav_panel("Batch", mod_batch_ui("batch")),
  bslib::nav_panel("Method Comparison", mod_compare_ui("compare")),
  bslib::nav_panel("Documentation", mod_documentation_ui("doc"))
)

server <- function(input, output, session) {
  mod_single_server("single")
  mod_batch_server("batch")
  mod_compare_server("compare")
}

shinyApp(ui, server)
