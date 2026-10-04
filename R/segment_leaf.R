#' Segment the leaf and classify its tissue (automatic or manual)
#'
#' Performs, in this order: optional conservative normalisation, **leaf /
#' background segmentation** (the leaf mask), and **tissue classification
#' restricted to pixels inside the leaf mask**. Background pixels can never be
#' counted as healthy or injured tissue.
#'
#' @section Automatic mode:
#' Thresholds are derived from each image (see *Segmentation methods*) and
#' cleaned with morphology and connected-component analysis. Every decision
#' (thresholds, removed components, filled holes) is stored in the result.
#'
#' @section Manual mode:
#' Starts from an automatic segmentation (or from `segmentation`, if given)
#' and applies user `corrections` in order. `corrections` is a data frame with
#' one row per correction and the columns:
#' \describe{
#'   \item{`action`}{`"add_leaf"`, `"remove_leaf"`, `"healthy"`, `"injured"`,
#'     `"chlorotic"` or `"necrotic"` (the last two need `multiclass = TRUE`;
#'     otherwise they mean `"injured"`).}
#'   \item{`shape`}{`"circle"` (a brush stroke: `x`, `y`, `radius`) or
#'     `"rect"` (`xmin`, `xmax`, `ymin`, `ymax`).}
#' }
#' Coordinates are pixels of the image being segmented. Pixels added to the
#' leaf are classified with the same tissue rule; tissue actions only change
#' pixels inside the leaf mask. Corrections are stored in the result, so a
#' corrected analysis is reproducible.
#'
#' @section Segmentation methods (`method`):
#' `"auto"` (default; Otsu thresholds on CIELAB chroma and darkness with
#' background polarity inferred from the image border, union of accepted
#' features, k-means fallback), `"lab"` (chroma), `"hsv"` (saturation),
#' `"exg"` (excess green; excludes brown necrosis, provided for comparison),
#' `"otsu"` (grey level), `"adaptive"` (lightness after removing a fitted
#' illumination surface), `"kmeans"` (clustering in CIELAB).
#'
#' @section Tissue methods (`tissue_method`):
#' `"lab"` (default; fixed CIELAB hue-angle thresholds, identical for all
#' images, preferable when comparing treatments), `"hsv"`, `"exgr"`
#' (ExG - ExR; mainly detects necrosis), `"vote"` (majority of lab, hsv and
#' exgr), `"auto"` (within-leaf Otsu on hue, bounded) and `"relative"`
#' (distance to the leaf's own dominant colour; intended for naturally
#' pigmented or senescent leaves, where "absence of green" is not a valid
#' injury criterion). Achromatic pixels are never healthy. With
#' `multiclass = TRUE`, injured pixels are subdivided into chlorotic,
#' necrotic and other, and `injured = chlorotic + necrotic + other` exactly.
#'
#' Thresholds are starting values, not validated constants; calibrate them
#' against manual reference masks for each imaging setup (see
#' [validate_leaf()]).
#'
#' @param image Path to an image file or an EBImage `Image` (typically the
#'   output of [crop_leaf()]).
#' @param mode `"auto"` or `"manual"`.
#' @param method Leaf/background segmentation method.
#' @param tissue_method Tissue classification method.
#' @param multiclass Logical; classify healthy/chlorotic/necrotic/other.
#' @param normalize Logical; apply conservative white balance estimated from
#'   background pixels before segmentation (original pixels are preserved).
#' @param corrections Data frame of manual corrections (manual mode).
#' @param segmentation Optional existing `leaf_segmentation` to correct
#'   (avoids recomputing the automatic step).
#' @param params Optional parameter overrides (see [analyze_leaf()]).
#' @return An object of class `leaf_segmentation` with `leaf_mask`,
#'   `class_map` (0 background, 1 healthy, 2 injured or 2 chlorotic /
#'   3 necrotic / 4 other), `healthy_mask`, `injury_mask`,
#'   `perforation_mask`, `metrics` (pixel counts and percentages),
#'   `thresholds`, `details`, `components` (with removal reasons), `holes`,
#'   `corrections`, `mode`, `method`, `tissue_method`, `multiclass`,
#'   `normalization` and `config`.
#' @examples
#' f <- system.file("extdata", "leaf_example_02.jpg", package = "LeafInjuryR")
#' img <- crop_leaf(f)
#' seg <- segment_leaf(img)
#' seg$metrics
#'
#' # manual correction: mark a rectangle as healthy, remove a brush stroke
#' fix <- data.frame(action = c("healthy", "remove_leaf"),
#'                   shape = c("rect", "circle"),
#'                   xmin = c(50, NA), xmax = c(120, NA),
#'                   ymin = c(200, NA), ymax = c(260, NA),
#'                   x = c(NA, 30), y = c(NA, 30), radius = c(NA, 15))
#' seg2 <- segment_leaf(img, mode = "manual", corrections = fix, segmentation = seg)
#' seg2$metrics$injured_percent
#' @export
segment_leaf <- function(image, mode = c("auto", "manual"), method = "auto",
                         tissue_method = "lab", multiclass = FALSE, normalize = FALSE,
                         corrections = NULL, segmentation = NULL, params = list()) {
  mode <- match.arg(mode)
  config <- params_to_config(params)
  method <- match.arg(method, leaf_methods()$method)
  tissue_method <- match.arg(tissue_method, leaf_methods()$tissue_method)
  img <- resolve_image(image)

  if (!is.null(segmentation)) {
    if (!inherits(segmentation, "leaf_segmentation")) {
      leaf_abort("`segmentation` must be a 'leaf_segmentation' object from segment_leaf().")
    }
    if (!identical(as.integer(dim(segmentation$leaf_mask)), as.integer(dim(img)[1:2]))) {
      leaf_abort("`segmentation` was computed on an image of a different size.")
    }
    seg <- segmentation
    config <- seg$config
  } else {
    seg <- auto_segment(img, method, tissue_method, multiclass, normalize, config)
  }
  if (mode == "manual") {
    if (is.null(corrections) || !nrow(as.data.frame(corrections))) {
      leaf_abort("Manual mode needs `corrections` (see ?segment_leaf).")
    }
    work <- seg$normalized %||% img
    seg <- apply_corrections(seg, as_rgb_array(work), corrections, config)
  }
  seg
}

#' Automatic segmentation + classification
#' @noRd
auto_segment <- function(img, method, tissue_method, multiclass, normalize, config) {
  normalized <- if (isTRUE(normalize)) normalize_leaf_image(img, config = config) else NULL
  arr <- as_rgb_array(normalized %||% img)
  m <- segment_leaf_mask(arr, method = method, config = config)
  tis <- classify_leaf_tissue(arr, m$mask, method = tissue_method,
                              multiclass = multiclass, config = config)
  metrics <- if (any(m$mask)) calculate_injury(tis$class_map, m$mask, multiclass) else
    empty_metrics(multiclass)
  structure(list(
    leaf_mask = m$mask, class_map = tis$class_map,
    healthy_mask = tis$healthy_mask, injury_mask = tis$injury_mask,
    perforation_mask = m$holes$perforation_mask, raw_mask = m$raw_mask,
    metrics = metrics,
    thresholds = list(segmentation = m$thresholds, tissue = tis$thresholds),
    separability = m$separability,
    details = list(segmentation = m$details, tissue = tis$details),
    components = m$components,
    holes = m$holes[setdiff(names(m$holes), "perforation_mask")],
    background_lab = m$background_lab, n_raw_components = m$n_raw_components,
    main_component_share = m$main_component_share,
    min_object_size_used = m$min_object_size_used,
    corrections = as_corrections(NULL), mode = "auto",
    method = method, tissue_method = tissue_method, multiclass = isTRUE(multiclass),
    normalized = normalized,
    normalization = if (!is.null(normalized)) attr(normalized, "normalization") else NULL,
    config = config
  ), class = "leaf_segmentation")
}

#' Metrics row for an empty leaf mask
#' @noRd
empty_metrics <- function(multiclass) {
  out <- data.frame(leaf_pixels = 0L, healthy_pixels = 0L, injured_pixels = 0L,
                    healthy_percent = NA_real_, injured_percent = NA_real_)
  if (isTRUE(multiclass)) {
    out$chlorotic_pixels <- 0L; out$necrotic_pixels <- 0L; out$other_pixels <- 0L
    out$chlorotic_percent <- NA_real_; out$necrotic_percent <- NA_real_
    out$other_percent <- NA_real_
  }
  out
}

#' @export
print.leaf_segmentation <- function(x, ...) {
  m <- x$metrics
  cat("<leaf_segmentation>", x$mode, "| method:", x$method, "| tissue:", x$tissue_method,
      if (x$multiclass) "| multiclass" else "", "\n")
  cat(sprintf("  leaf pixels: %d | healthy: %.2f%% | injured: %.2f%%\n",
              as.integer(m$leaf_pixels), m$healthy_percent, m$injured_percent))
  if (nrow(x$corrections)) cat("  manual corrections:", nrow(x$corrections), "\n")
  invisible(x)
}

#' @export
plot.leaf_segmentation <- function(x, ...) {
  op <- graphics::par(mfrow = c(1, 2)); on.exit(graphics::par(op))
  plot_leaf_image(x$leaf_mask, main = "Leaf mask")
  plot_leaf_image(class_map_to_image(x$class_map, x$multiclass), main = "Classification")
  invisible(x)
}
