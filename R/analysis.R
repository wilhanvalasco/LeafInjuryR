#' Analyse injury on a single leaf image
#'
#' Complete, auditable pipeline for one photograph:
#'
#' ```
#' import -> crop (optional) -> segment leaf -> leaf mask -> classify tissue
#'   INSIDE the leaf mask -> (manual corrections) -> quantify -> quality control
#' ```
#'
#' The injury percentage is `injured pixels / leaf-mask pixels x 100`; the
#' background never enters the calculation. Areas are reported in **pixels**.
#' Physical areas (cm^2) are added only when a valid scale is supplied with
#' `pixels_per_cm` (e.g. measured from a ruler or reference object photographed
#' in the same plane as the leaf).
#'
#' @section Parameters (`params`):
#' Every tunable value has a documented default and can be overridden with a
#' named list of groups, e.g.
#' `params = list(morphology = list(min_object_size = 500), tissue = list(lab_healthy_hue_min = 98))`.
#' The complete configuration used is stored in `result$parameters$config`
#' and can be passed back as `params` to reproduce an analysis.
#' \describe{
#'   \item{`segmentation`}{`threshold` (manual threshold for single-feature
#'     methods; `NULL` = image-derived), `otsu_bins` (256), `min_separability`
#'     (0.5), `border_fraction` (0.05), `max_border_foreground` (0.35),
#'     `kmeans_centers` (3), `kmeans_sample` (20000), `kmeans_nstart` (5),
#'     `kmeans_iter` (50), `kmeans_background_border_share` (0.2),
#'     `surface_degree` (2), `surface_sample` (20000), `seed` (1).}
#'   \item{`morphology`}{`opening_radius` (1 px), `brush_shape` ("disc"),
#'     `min_object_size` (`NULL` = `min_object_fraction` x image area),
#'     `min_object_fraction` (0.001), `keep_largest` (TRUE), `fill_holes`
#'     (TRUE), `hole_policy` ("fill_non_background", "fill_all", "none"),
#'     `background_delta_e` (15), `min_perforation_size` (25 px).}
#'   \item{`crop`}{`margin_fraction` (0.05), `preview_max_dim` (400).}
#'   \item{`normalization`}{`white_balance` (TRUE), `flatten_illumination`
#'     (FALSE), `background_quantile` (0.5), `max_gain` (1.3).}
#'   \item{`tissue`}{`lab_healthy_hue_min` (100), `lab_healthy_hue_max` (200),
#'     `lab_necrotic_hue_max` (85), `hsv_healthy_hue_min` (57),
#'     `hsv_healthy_hue_max` (170), `hsv_min_saturation` (0.15),
#'     `exgr_threshold` (0), `achromatic_chroma_max` (8),
#'     `dark_lightness_max` (35), `auto_hue_bounds` (90, 110),
#'     `relative_hue_shift` (15), `relative_lightness_drop` (25),
#'     `relative_bin_width` (4), `min_injury_size` (0).}
#'   \item{`qc`}{`min_leaf_fraction` (0.02), `max_leaf_fraction` (0.90),
#'     `max_raw_components` (50), `min_main_component_share` (0.90),
#'     `min_contrast` (0.5), `max_illumination_cv` (0.08),
#'     `illumination_grid` (4), `near_zero_injury` (0.5),
#'     `near_full_injury` (99.5), `max_other_percent` (5).}
#' }
#' Hue angles are in degrees. All defaults are starting values, not validated
#' constants.
#'
#' @param image Path to an image file (JPEG, PNG, TIFF) or an EBImage `Image`.
#' @param crop `"none"`, `"auto"` or `"manual"` (see [crop_leaf()]).
#' @param crop_points For `crop = "manual"`: list with numeric `x` and `y`.
#' @param method,tissue_method,multiclass,normalize See [segment_leaf()].
#' @param corrections Optional manual corrections (see [segment_leaf()]),
#'   in coordinates of the cropped image.
#' @param segmentation Optional `leaf_segmentation` already computed on the
#'   cropped image (e.g. corrected interactively); it is used as is.
#' @param pixels_per_cm Optional scale (pixels per centimetre). If supplied,
#'   areas in cm^2 are added.
#' @param params Optional parameter overrides (see *Parameters*).
#' @param keep_images Keep image arrays in the result (set `FALSE` to save
#'   memory; masks and metrics are always kept).
#' @param display Plot the four audit panels.
#' @return An object of class `leaf_analysis` with `original`, `cropped`,
#'   `normalized`, `leaf_mask`, `healthy_mask`, `injury_mask`, `class_map`,
#'   `perforation_mask`, `overlay`, `metrics` (one-row data frame),
#'   `parameters` (complete record for reproducibility), `quality`,
#'   `metadata`, `crop`, `segmentation` and `timing`. Has `print()`,
#'   `plot()` and `as.data.frame()` methods.
#' @examples
#' f <- system.file("extdata", "leaf_example_02.jpg", package = "LeafInjuryR")
#' res <- analyze_leaf(f, crop = "auto")
#' res
#' res$metrics[, c("leaf_pixels", "injured_pixels", "injured_percent", "quality_flag")]
#'
#' res_mc <- analyze_leaf(f, crop = "auto", multiclass = TRUE)
#' res_mc$metrics[, c("chlorotic_percent", "necrotic_percent", "other_percent")]
#' @export
analyze_leaf <- function(image, crop = c("none", "auto", "manual"), crop_points = NULL,
                         method = "auto", tissue_method = "lab", multiclass = FALSE,
                         normalize = FALSE, corrections = NULL, segmentation = NULL,
                         pixels_per_cm = NULL, params = list(), keep_images = TRUE,
                         display = FALSE) {
  t_start <- Sys.time()
  crop <- match.arg(crop)
  config <- if (!is.null(segmentation) && inherits(segmentation, "leaf_segmentation"))
    segmentation$config else params_to_config(params)
  if (!is.null(pixels_per_cm) &&
      (!is.numeric(pixels_per_cm) || length(pixels_per_cm) != 1L || !is.finite(pixels_per_cm) ||
       pixels_per_cm <= 0)) {
    leaf_abort("`pixels_per_cm` must be a single positive number (or NULL when no scale is available).")
  }
  file_md5 <- if (is.character(image) && length(image) == 1L && file.exists(image))
    unname(tools::md5sum(image)) else NA_character_

  original <- resolve_image(image)
  metadata <- leaf_image_metadata(original)
  d0 <- dim(original)
  cropped <- switch(crop,
    none = original,
    auto = crop_leaf(original, "auto", method = method, params = config),
    manual = {
      if (is.null(crop_points)) leaf_abort("`crop_points` is required when crop = 'manual'.")
      crop_leaf(original, "manual", x = crop_points$x, y = crop_points$y)
    })
  crop_info <- attr(cropped, "crop_info") %||%
    list(method = "none", found = NA,
         bbox = c(x_min = 1, x_max = d0[1], y_min = 1, y_max = d0[2]), original_dim = d0[1:2])
  crop_info$method <- crop

  has_corr <- !is.null(corrections) && nrow(as.data.frame(corrections)) > 0
  seg <- segment_leaf(cropped, mode = if (has_corr) "manual" else "auto", method = method,
                      tissue_method = tissue_method, multiclass = multiclass,
                      normalize = normalize, corrections = if (has_corr) corrections,
                      segmentation = segmentation, params = config)
  work <- seg$normalized %||% cropped
  arr <- as_rgb_array(work)
  mc <- seg$multiclass

  metrics <- cbind(
    data.frame(file = metadata$filename %||% NA_character_, method = seg$method,
               tissue_method = seg$tissue_method, multiclass = mc, stringsAsFactors = FALSE),
    seg$metrics)
  quality <- compute_quality(arr, qc_view(seg), metrics, config)
  metrics$perforation_candidate_pixels <- seg$holes$perforation_candidate_pixels %||% 0L
  metrics$manual_corrections <- nrow(seg$corrections)
  if (!is.null(pixels_per_cm)) {
    px_area <- 1 / pixels_per_cm^2
    metrics$pixels_per_cm <- pixels_per_cm
    metrics$leaf_area_cm2 <- metrics$leaf_pixels * px_area
    metrics$healthy_area_cm2 <- metrics$healthy_pixels * px_area
    metrics$injured_area_cm2 <- metrics$injured_pixels * px_area
    if (mc) {
      metrics$chlorotic_area_cm2 <- metrics$chlorotic_pixels * px_area
      metrics$necrotic_area_cm2 <- metrics$necrotic_pixels * px_area
    }
  }
  metrics$quality_flag <- quality$quality_flag
  metrics$diagnostic_score <- quality$diagnostic_score
  metrics$crop_method <- crop
  metrics$image_width <- d0[1]
  metrics$image_height <- d0[2]
  metrics$analyzed_width <- dim(arr)[1]
  metrics$analyzed_height <- dim(arr)[2]
  metrics$file_md5 <- file_md5

  overlay <- if (isTRUE(keep_images) || isTRUE(display))
    create_leaf_overlay(cropped, seg$class_map, alpha = 0.45, multiclass = mc) else NULL
  parameters <- build_parameters(metadata, crop, crop_info, normalize, seg, config,
                                 pixels_per_cm, file_md5)
  metrics$analysis_timestamp <- parameters$analysis_timestamp
  metrics$package_version <- parameters$package_version

  seg_store <- seg
  seg_store$normalized <- NULL
  res <- structure(list(
    original = if (keep_images) original else NULL,
    cropped = if (keep_images) cropped else NULL,
    normalized = if (keep_images) seg$normalized else NULL,
    leaf_mask = seg$leaf_mask, healthy_mask = seg$healthy_mask,
    injury_mask = seg$injury_mask, class_map = seg$class_map,
    perforation_mask = seg$perforation_mask,
    overlay = if (keep_images) overlay else NULL,
    metrics = metrics, parameters = parameters, quality = quality,
    metadata = metadata, crop = crop_info, segmentation = seg_store,
    timing = list(elapsed_sec = as.numeric(difftime(Sys.time(), t_start, units = "secs")))
  ), class = "leaf_analysis")
  if (isTRUE(display)) {
    if (!keep_images) { res$cropped <- cropped; res$overlay <- overlay }
    graphics::plot(res)
  }
  res
}

#' View of a leaf_segmentation in the shape expected by compute_quality()
#' @noRd
qc_view <- function(seg) {
  list(mask = seg$leaf_mask, raw_mask = seg$raw_mask %||% seg$leaf_mask,
       separability = seg$separability %||% list(),
       thresholds = seg$thresholds$segmentation %||% list(),
       n_raw_components = seg$n_raw_components %||% NA_integer_,
       main_component_share = seg$main_component_share %||% NA_real_,
       holes = seg$holes %||% list(perforation_candidate_pixels = 0L))
}

#' @noRd
build_parameters <- function(metadata, crop, crop_info, normalize, seg, config,
                             pixels_per_cm, file_md5) {
  si <- Sys.info()
  list(
    package_version = pkg_version("LeafInjuryR"),
    R_version = paste(R.version$major, R.version$minor, sep = "."),
    EBImage_version = pkg_version("EBImage"),
    platform = paste(si[["sysname"]], si[["release"]], R.version$arch),
    analysis_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    file = metadata$filename %||% NA_character_,
    file_md5 = file_md5,
    background_method = seg$method,
    background_strategy = seg$details$segmentation$strategy %||% seg$method,
    tissue_method = seg$tissue_method,
    multiclass = isTRUE(seg$multiclass),
    segmentation_mode = seg$mode,
    normalization = list(enabled = isTRUE(normalize),
                         applied = seg$normalization$applied %||% character(0),
                         white_balance_gains = seg$normalization$white_balance_gains),
    crop_method = crop,
    crop_bbox = crop_info$bbox,
    crop_found = crop_info$found,
    thresholds = list(segmentation = seg$thresholds$segmentation,
                      segmentation_source = if (is.null(config$segmentation$threshold))
                        "image-derived" else "user",
                      tissue = seg$thresholds$tissue),
    separability = seg$separability,
    segmentation_details = seg$details$segmentation,
    morphological_parameters = c(config$morphology,
                                 list(min_object_size_used = seg$min_object_size_used)),
    removed_components = if (is.data.frame(seg$components) && nrow(seg$components))
      seg$components[!seg$components$kept, , drop = FALSE] else data.frame(),
    hole_handling = seg$holes,
    manual_corrections = seg$corrections,
    pixels_per_cm = pixels_per_cm,
    area_unit = if (is.null(pixels_per_cm)) "pixels" else "pixels and cm^2",
    quality_control_parameters = config$qc,
    seed = config$segmentation$seed,
    config = config
  )
}

#' @export
print.leaf_analysis <- function(x, ...) {
  m <- x$metrics
  cat("<leaf_analysis>", if (!is.na(m$file)) m$file else "", "\n")
  cat(sprintf("  segmentation: %s (%s) | tissue: %s | multiclass: %s\n",
              m$method, x$parameters$background_strategy, m$tissue_method, m$multiclass))
  cat(sprintf("  leaf pixels: %d | healthy: %.2f%% | injured: %.2f%%\n",
              as.integer(m$leaf_pixels), m$healthy_percent, m$injured_percent))
  if (isTRUE(m$multiclass)) {
    cat(sprintf("  chlorotic: %.2f%% | necrotic: %.2f%% | other: %.2f%%\n",
                m$chlorotic_percent, m$necrotic_percent, m$other_percent))
  }
  if (!is.null(m$leaf_area_cm2)) cat(sprintf("  leaf area: %.2f cm2\n", m$leaf_area_cm2))
  if (m$manual_corrections > 0) cat("  manual corrections:", m$manual_corrections, "\n")
  cat("  quality:", x$quality$quality_flag, "\n")
  for (msg in x$quality$quality_messages) cat("   -", msg, "\n")
  invisible(x)
}

#' @export
as.data.frame.leaf_analysis <- function(x, ...) x$metrics
