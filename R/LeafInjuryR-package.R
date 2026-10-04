#' LeafInjuryR: quantification of visual leaf injury patterns
#'
#' Classical, auditable image processing for the percentage of injured leaf
#' area, with six public functions:
#'
#' | Function | Purpose |
#' |---|---|
#' | [analyze_leaf()] | single-image analysis |
#' | [analyze_leaf_batch()] | batch analysis with HTML, Excel and CSV reports |
#' | [crop_leaf()] | automatic and manual cropping |
#' | [segment_leaf()] | automatic and manually corrected segmentation |
#' | [validate_leaf()] | quality control and validation against reference masks |
#' | [run_leafinjury_app()] | local Shiny interface |
#'
#' Leaf segmentation (which pixels are leaf) is always performed before, and
#' separately from, tissue classification (which leaf pixels are healthy or
#' injured). Injury (%) = injured pixels / leaf-mask pixels x 100.
#'
#' **Scope.** The package quantifies visual colour patterns. It does not
#' diagnose pathogens, diseases, nutritional deficiencies or phytotoxicity.
#' Working software is not the same as a validated method: calibrate and
#' validate against manual reference masks for your own imaging conditions.
#'
#' **Privacy.** All processing is local; no telemetry or external services.
#'
#' @keywords internal
"_PACKAGE"
