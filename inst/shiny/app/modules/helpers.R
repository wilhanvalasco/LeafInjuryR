# Shared helpers for the Shiny modules (UI glue only; no analysis logic here).

method_choices <- c("Auto" = "auto", "CIELAB chroma" = "lab", "HSV saturation" = "hsv",
                    "Excess green (ExG)" = "exg", "Otsu (grey)" = "otsu",
                    "Adaptive (illumination-corrected)" = "adaptive", "k-means (Lab)" = "kmeans")
tissue_choices <- c("CIELAB hue (fixed thresholds)" = "lab", "HSV hue/saturation" = "hsv",
                    "ExG - ExR" = "exgr", "Majority vote" = "vote",
                    "Auto (within-leaf Otsu, bounded)" = "auto")

# Advanced-settings controls, shared by single and batch modules
advanced_controls <- function(ns) {
  defaults <- LeafInjuryR::leaf_config()
  tagList(
    selectInput(ns("method"), "Background (leaf segmentation) method", method_choices, "auto"),
    selectInput(ns("tissue_method"), "Tissue classification method", tissue_choices, "lab"),
    checkboxInput(ns("normalize"), "Illumination/colour normalisation", FALSE),
    checkboxInput(ns("multiclass"), "Multiclass (healthy/chlorotic/necrotic/other)", FALSE),
    numericInput(ns("threshold"), "Manual segmentation threshold (blank = image-derived)", NA),
    numericInput(ns("min_object_size"), "Minimum object size, px (blank = relative default)", NA, min = 0),
    numericInput(ns("opening_radius"), "Morphological opening radius (px)",
                 defaults$morphology$opening_radius, min = 0, max = 10),
    selectInput(ns("hole_policy"), "Hole handling",
                c("Fill non-background holes" = "fill_non_background",
                  "Fill all holes" = "fill_all", "Do not fill" = "none")),
    numericInput(ns("healthy_hue"), "Lab hue: healthy lower limit (deg)",
                 defaults$tissue$lab_healthy_hue_min, min = 0, max = 360),
    numericInput(ns("necrotic_hue"), "Lab hue: necrotic upper limit (deg)",
                 defaults$tissue$lab_necrotic_hue_max, min = 0, max = 360)
  )
}

# Build analysis arguments from inputs (basic mode = package defaults)
analysis_args <- function(input, advanced) {
  num <- function(v) if (is.null(v) || is.na(v)) NULL else v
  if (!advanced) {
    return(list(method = "auto", tissue_method = "lab", normalize = FALSE,
                multiclass = isTRUE(input$multiclass_basic), threshold = NULL,
                config = LeafInjuryR::leaf_config()))
  }
  cfg <- LeafInjuryR::leaf_config(
    morphology = list(opening_radius = input$opening_radius %||% 1,
                      min_object_size = num(input$min_object_size),
                      fill_holes = input$hole_policy != "none",
                      hole_policy = input$hole_policy),
    tissue = list(lab_healthy_hue_min = input$healthy_hue,
                  lab_necrotic_hue_max = input$necrotic_hue))
  list(method = input$method, tissue_method = input$tissue_method,
       normalize = isTRUE(input$normalize), multiclass = isTRUE(input$multiclass),
       threshold = num(input$threshold), config = cfg)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

fmt_pct <- function(x) if (is.null(x) || is.na(x)) "-" else sprintf("%.2f %%", x)

quality_badge <- function(flag) {
  col <- switch(flag %||% "", ok = "#1FA83A", warning = "#E0A800", failed = "#D62728", "#777777")
  tags$span(style = sprintf("color:%s;font-weight:700;", col), toupper(flag %||% "-"))
}

# Zip a set of files into `zipfile`; returns TRUE on success
safe_zip <- function(zipfile, files) {
  ok <- tryCatch({
    utils::zip(zipfile, files = files, flags = "-j -q")
    file.exists(zipfile)
  }, error = function(e) FALSE, warning = function(w) file.exists(zipfile))
  isTRUE(ok)
}

# Zip a whole directory (keeping sub-folders) into `zipfile`
safe_zip_dir <- function(zipfile, dir) {
  old <- setwd(dir); on.exit(setwd(old))
  ok <- tryCatch({
    utils::zip(zipfile, files = list.files(".", recursive = TRUE), flags = "-q")
    file.exists(zipfile)
  }, error = function(e) FALSE, warning = function(w) file.exists(zipfile))
  isTRUE(ok)
}
