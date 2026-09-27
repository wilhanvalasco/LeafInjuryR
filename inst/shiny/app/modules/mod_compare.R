# Method comparison: same image, several segmentation methods side by side

mod_compare_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      fileInput(ns("file"), "Image", accept = c(".jpg", ".jpeg", ".png", ".tif", ".tiff")),
      actionLink(ns("example1"), "Example 1"), " | ", actionLink(ns("example2"), "Example 2"),
      checkboxGroupInput(ns("methods"), "Methods", method_choices,
                         selected = c("hsv", "lab", "exg", "otsu", "auto")),
      actionButton(ns("run"), "Compare", class = "btn-primary w-100")
    ),
    tags$div(class = "alert alert-info",
             "Agreement between methods is not accuracy. No method is declared the best ",
             "without manual reference masks (ground truth)."),
    bslib::card(bslib::card_header("Original and leaf masks"),
                plotOutput(ns("grid"), height = "620px")),
    bslib::layout_columns(
      bslib::card(bslib::card_header("Areas and heuristic indicators"), tableOutput(ns("summary"))),
      bslib::card(bslib::card_header("Pairwise IoU between methods (agreement)"),
                  tableOutput(ns("agree")))
    )
  )
}

mod_compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    img <- reactiveVal(NULL); cmp <- reactiveVal(NULL)
    set_img <- function(path) {
      im <- tryCatch(LeafInjuryR::read_leaf(path), error = function(e) {
        showNotification(conditionMessage(e), type = "error"); NULL })
      img(im); cmp(NULL)
    }
    observeEvent(input$file, set_img(input$file$datapath))
    observeEvent(input$example1, set_img(LeafInjuryR::leaf_example_images()[1]))
    observeEvent(input$example2, set_img(LeafInjuryR::leaf_example_images()[2]))
    observeEvent(input$run, {
      req(img(), length(input$methods) > 0)
      withProgress(message = "Running methods...", value = 0.5, {
        cmp(tryCatch(LeafInjuryR::compare_segmentation_methods(img(), methods = input$methods),
                     error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL }))
      })
    })
    output$grid <- renderPlot({ req(cmp()); plot(cmp()) })
    output$summary <- renderTable({
      req(cmp()); s <- cmp()$summary
      s$leaf_fraction <- round(100 * s$leaf_fraction, 2)
      names(s)[names(s) == "leaf_fraction"] <- "leaf_%_of_image"
      s
    }, digits = 3)
    output$agree <- renderTable(round(cmp()$agreement, 3), rownames = TRUE)
  })
}
