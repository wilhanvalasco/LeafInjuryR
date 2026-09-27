#' Centralised analysis configuration
#'
#' All tunable parameters of the pipeline live in a single structured object,
#' so that no "magic numbers" are scattered through the code, every value has a
#' documented default, and the exact configuration used in an analysis is
#' stored in the result (see [get_analysis_parameters()]).
#'
#' `leaf_config()` returns the defaults, optionally modified. Modifications
#' are given as named lists per group; only the supplied entries are replaced.
#'
#' @section Segmentation (`segmentation`):
#' \describe{
#'   \item{`otsu_bins`}{Histogram bins used by Otsu thresholding (256).}
#'   \item{`min_separability`}{Minimum Otsu separability (between-class /
#'     total variance, 0-1) for a colour feature to be used by
#'     `method = "auto"` (0.5). Heuristic.}
#'   \item{`border_fraction`}{Fraction of each image side treated as the
#'     border region used to infer which side of a threshold is background
#'     (0.05).}
#'   \item{`max_border_foreground`}{In `"auto"`, a feature is rejected if more
#'     than this fraction of border pixels would be foreground (0.35).}
#'   \item{`kmeans_sample`}{Pixels sampled for k-means clustering (20000).}
#'   \item{`kmeans_iter`}{Maximum k-means iterations (50).}
#'   \item{`kmeans_centers`}{Number of k-means clusters (3: background, leaf
#'     and a third colour group such as necrosis or shadow).}
#'   \item{`kmeans_nstart`}{Random starts of k-means (5; seeded).}
#'   \item{`kmeans_background_border_share`}{A cluster holding at least this
#'     share of border pixels is background (0.2).}
#'   \item{`surface_degree`}{Polynomial degree of the background illumination
#'     surface used by `method = "adaptive"` and illumination flattening (2).}
#'   \item{`surface_sample`}{Background pixels sampled to fit that surface (20000).}
#'   \item{`seed`}{Random seed for every sampling step (1).}
#' }
#'
#' @section Morphology (`morphology`):
#' \describe{
#'   \item{`opening_radius`}{Radius (pixels) of the morphological opening
#'     applied to the raw leaf mask; 0 disables it (1).}
#'   \item{`brush_shape`}{Structuring element shape passed to
#'     [EBImage::makeBrush()] ("disc").}
#'   \item{`min_object_size`}{Minimum connected-component area in pixels. If
#'     `NULL`, `min_object_fraction` x image area is used (NULL).}
#'   \item{`min_object_fraction`}{Relative minimum component size (0.001).}
#'   \item{`keep_largest`}{Keep only the largest component (the main leaf)
#'     (TRUE). Removed components are always recorded.}
#'   \item{`fill_holes`}{Fill internal holes of the leaf mask (TRUE).}
#'   \item{`hole_policy`}{"fill_non_background" (default): only holes whose
#'     colour differs from the estimated background colour are filled; holes
#'     that look like background are kept out of the leaf and reported as
#'     perforation candidates (experimental). "fill_all": fill every hole.
#'     "none": never fill.}
#'   \item{`background_delta_e`}{CIE76 colour distance (Delta E) below which a
#'     hole pixel is considered background-coloured (15).}
#'   \item{`min_perforation_size`}{Background-coloured holes smaller than this
#'     (pixels) are filled, since they are more likely glare or specks (25).}
#' }
#'
#' @section Cropping (`crop`):
#' \describe{
#'   \item{`margin_fraction`}{Margin added around the automatic bounding box,
#'     as a fraction of its size (0.05).}
#'   \item{`preview_max_dim`}{Maximum dimension of the down-scaled preview used
#'     to locate the leaf for automatic cropping (400).}
#' }
#'
#' @section Normalisation (`normalization`):
#' \describe{
#'   \item{`white_balance`}{Neutralise colour cast using background pixels (TRUE).}
#'   \item{`flatten_illumination`}{Correct smooth illumination gradients using
#'     a polynomial surface fitted to background pixels (FALSE).}
#'   \item{`background_quantile`}{Only background pixels brighter than this
#'     quantile of background lightness serve as white reference (0.5).}
#'   \item{`max_gain`}{Maximum multiplicative correction per channel/pixel;
#'     larger corrections are clipped to keep the correction conservative (1.3).}
#' }
#'
#' @section Tissue classification (`tissue`):
#' Hue angles are in degrees. CIELAB hue angle h = atan2(b*, a*); HSV hue is
#' the usual 0-360 hue.
#' \describe{
#'   \item{`lab_healthy_hue_min`}{Lab hue angle at or above which a chromatic
#'     pixel is healthy (100).}
#'   \item{`lab_healthy_hue_max`}{Upper Lab hue angle of healthy tissue;
#'     injured pixels with hue between this value and 300 are "other",
#'     from 300 upwards (reddish-purple) necrotic (200).}
#'   \item{`lab_necrotic_hue_max`}{Lab hue angle below which an injured pixel
#'     is necrotic; injured pixels between this and `lab_healthy_hue_min` are
#'     chlorotic (85).}
#'   \item{`hsv_healthy_hue_min`, `hsv_healthy_hue_max`}{HSV hue range regarded
#'     as healthy (57, 170). The default 57 was chosen to correspond approximately to Lab hue 100 on the example images (consistency between rules, not validation).}
#'   \item{`hsv_min_saturation`}{Pixels with lower HSV saturation are never
#'     healthy (0.15).}
#'   \item{`exgr_threshold`}{ExG - ExR above which a pixel is healthy (0).}
#'   \item{`achromatic_chroma_max`}{Lab chroma below which a pixel is treated as
#'     achromatic: never healthy; necrotic if dark, other if bright (8).}
#'   \item{`dark_lightness_max`}{L* below which an achromatic injured pixel is
#'     necrotic (35).}
#'   \item{`auto_hue_bounds`}{In `tissue_method = "auto"`, the Otsu threshold on
#'     Lab hue inside the leaf is constrained to this interval (90, 110).}
#'   \item{`min_injury_size`}{Injured connected regions smaller than this
#'     (pixels) are re-labelled healthy; 0 disables (0).}
#' }
#'
#' @section Quality control (`qc`):
#' All values are heuristic warning limits. Warnings never modify results.
#' \describe{
#'   \item{`min_leaf_fraction`}{Leaf smaller than this fraction of the image (0.02).}
#'   \item{`max_leaf_fraction`}{Leaf larger than this fraction of the image (0.90).}
#'   \item{`max_raw_components`}{Raw mask with more components than this (50).}
#'   \item{`min_main_component_share`}{Largest component holds less than this
#'     share of foreground (0.90) -> fragmented mask.}
#'   \item{`min_contrast`}{Separability below this -> low leaf/background contrast (0.5).}
#'   \item{`max_illumination_cv`}{Coefficient of variation of background
#'     lightness across grid tiles above this -> heterogeneous illumination (0.08).}
#'   \item{`illumination_grid`}{Grid size used for that check (4).}
#'   \item{`near_zero_injury`, `near_full_injury`}{Injury percent below / above
#'     these values triggers a warning (0.5, 99.5).}
#'   \item{`max_other_percent`}{"Other" class above this percent of the leaf
#'     triggers a warning in multiclass mode (5).}
#' }
#'
#' @param segmentation,morphology,crop,normalization,tissue,qc Named lists
#'   overriding entries of the corresponding group.
#' @return An object of class `leaf_config` (a nested list).
#' @seealso [get_analysis_parameters()]
#' @examples
#' cfg <- leaf_config()
#' cfg$morphology$min_object_fraction
#' cfg2 <- leaf_config(morphology = list(opening_radius = 2, keep_largest = FALSE))
#' @export
leaf_config <- function(segmentation = list(), morphology = list(), crop = list(),
                        normalization = list(), tissue = list(), qc = list()) {
  defaults <- default_config_values()
  overrides <- list(segmentation = segmentation, morphology = morphology,
                    crop = crop, normalization = normalization,
                    tissue = tissue, qc = qc)
  for (group in names(overrides)) {
    ov <- overrides[[group]]
    if (!is.list(ov)) leaf_abort(sprintf("`%s` must be a named list.", group))
    unknown <- setdiff(names(ov), names(defaults[[group]]))
    if (length(unknown)) {
      leaf_abort(sprintf("Unknown %s parameter(s): %s.", group,
                         paste(unknown, collapse = ", ")))
    }
    for (nm in names(ov)) defaults[[group]][nm] <- list(ov[[nm]])
  }
  cfg <- structure(defaults, class = c("leaf_config", "list"))
  validate_config(cfg)
  cfg
}

#' @noRd
default_config_values <- function() {
  list(
    segmentation = list(
      otsu_bins = 256L,
      min_separability = 0.5,
      border_fraction = 0.05,
      max_border_foreground = 0.35,
      kmeans_sample = 20000L,
      kmeans_iter = 50L,
      kmeans_centers = 3L,
      kmeans_nstart = 5L,
      kmeans_background_border_share = 0.2,
      surface_degree = 2L,
      surface_sample = 20000L,
      seed = 1L
    ),
    morphology = list(
      opening_radius = 1L,
      brush_shape = "disc",
      min_object_size = NULL,
      min_object_fraction = 0.001,
      keep_largest = TRUE,
      fill_holes = TRUE,
      hole_policy = "fill_non_background",
      background_delta_e = 15,
      min_perforation_size = 25L
    ),
    crop = list(
      margin_fraction = 0.05,
      preview_max_dim = 400L
    ),
    normalization = list(
      white_balance = TRUE,
      flatten_illumination = FALSE,
      background_quantile = 0.5,
      max_gain = 1.3
    ),
    tissue = list(
      lab_healthy_hue_min = 100,
      lab_necrotic_hue_max = 85,
      lab_healthy_hue_max = 200,
      hsv_healthy_hue_min = 57,
      hsv_healthy_hue_max = 170,
      hsv_min_saturation = 0.15,
      exgr_threshold = 0,
      achromatic_chroma_max = 8,
      dark_lightness_max = 35,
      auto_hue_bounds = c(90, 110),
      min_injury_size = 0L
    ),
    qc = list(
      min_leaf_fraction = 0.02,
      max_leaf_fraction = 0.90,
      max_raw_components = 50L,
      min_main_component_share = 0.90,
      min_contrast = 0.5,
      max_illumination_cv = 0.08,
      illumination_grid = 4L,
      near_zero_injury = 0.5,
      near_full_injury = 99.5,
      max_other_percent = 5
    )
  )
}

#' @noRd
validate_config <- function(cfg) {
  s <- cfg$segmentation; m <- cfg$morphology; t <- cfg$tissue
  chk <- function(ok, msg) if (!isTRUE(ok)) leaf_abort(paste("Invalid configuration:", msg))
  chk(s$min_separability >= 0 && s$min_separability <= 1, "min_separability must be in [0, 1].")
  chk(s$border_fraction > 0 && s$border_fraction < 0.5, "border_fraction must be in (0, 0.5).")
  chk(m$opening_radius >= 0, "opening_radius must be >= 0.")
  chk(m$hole_policy %in% c("fill_non_background", "fill_all", "none"),
      "hole_policy must be 'fill_non_background', 'fill_all' or 'none'.")
  chk(is.null(m$min_object_size) || m$min_object_size >= 0, "min_object_size must be >= 0.")
  chk(t$lab_necrotic_hue_max <= t$lab_healthy_hue_min,
      "lab_necrotic_hue_max must be <= lab_healthy_hue_min.")
  chk(length(t$auto_hue_bounds) == 2 && t$auto_hue_bounds[1] <= t$auto_hue_bounds[2],
      "auto_hue_bounds must be c(lower, upper).")
  chk(cfg$normalization$max_gain >= 1, "max_gain must be >= 1.")
  invisible(TRUE)
}

#' @export
print.leaf_config <- function(x, ...) {
  cat("<leaf_config>\n")
  for (g in names(x)) {
    cat(" ", g, ":\n", sep = "")
    for (nm in names(x[[g]])) {
      v <- x[[g]][[nm]]
      cat("    ", nm, " = ", if (is.null(v)) "NULL" else paste(v, collapse = ", "),
          "\n", sep = "")
    }
  }
  invisible(x)
}

#' @noRd
as_leaf_config <- function(config) {
  if (is.null(config)) return(leaf_config())
  if (!inherits(config, "leaf_config")) {
    leaf_abort("`config` must be created with leaf_config().")
  }
  config
}

#' Available method names
#'
#' @return A list with the valid values of `method` (leaf/background
#'   segmentation), `tissue_method` (tissue classification) and `crop`.
#' @examples
#' leaf_methods()
#' @export
leaf_methods <- function() {
  list(
    method = c("auto", "lab", "hsv", "exg", "otsu", "adaptive", "kmeans"),
    tissue_method = c("auto", "lab", "hsv", "exgr", "vote"),
    crop = c("none", "auto", "manual")
  )
}
