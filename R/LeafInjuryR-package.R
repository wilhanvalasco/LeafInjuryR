#' LeafInjuryR: quantification of visual leaf injury patterns
#'
#' Classical, auditable image processing for the percentage of injured leaf
#' area. The package separates:
#'
#' 1. **leaf segmentation** ([segment_leaf()]): which pixels belong to the
#'    leaf (including necrotic and chlorotic tissue);
#' 2. **tissue classification** ([classify_leaf_tissue()]): which leaf pixels
#'    look healthy or injured - computed only inside the leaf mask;
#' 3. **quantification** ([calculate_injury()]): percentages relative to leaf
#'    pixels.
#'
#' [analyze_leaf()] runs the full pipeline, [analyze_leaf_batch()] processes
#' many images and [run_leafinjury_app()] opens the local Shiny interface.
#' Validation against manual reference masks is supported by
#' [evaluate_segmentation()] and [validate_ground_truth()].
#'
#' **Scope.** The package quantifies *visual patterns* of colour change.
#' It does not diagnose pathogens, diseases, nutritional deficiencies or
#' phytotoxicity. Working software is not the same as a scientifically
#' validated method: validate on your own imaging conditions.
#'
#' **Privacy.** All processing is local; no telemetry or external services.
#'
#' @keywords internal
"_PACKAGE"
