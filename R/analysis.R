#' Analyse injury on a single leaf image
#'
#' Runs the complete, auditable pipeline:
#'
#' ```
#' original image -> (crop) -> (normalise) -> leaf segmentation -> leaf mask
#'   -> pixels INSIDE the leaf mask -> tissue classification -> quantification
#'   -> quality control
#' ```
#'
#' Injury classification happens strictly after segmentation, and no pixel
#' outside the leaf mask contributes to `healthy_pixels`, `injured_pixels` or
#' `injured_percent`.
#'
#' @param image Path to an image file or an EBImage `Image`.
#' @param method Leaf/background segmentation method (see [segment_leaf()]).
#' @param tissue_method Tissue classification method (see
#'   [classify_leaf_tissue()]).
#' @param crop `"none"`, `"auto"` ([auto_crop_leaf()]) or `"manual"`
#'   ([crop_leaf_manual()], requires `crop_points`).
#' @param crop_points For `crop = "manual"`: a list or data frame with
#'   numeric `x` and `y` pixel coordinates (at least two points).
#' @param normalize Logical; apply [normalize_leaf_image()] before
#'   segmentation and classification. The original pixels are kept.
#' @param multiclass Logical; classify healthy/chlorotic/necrotic/other.
#' @param threshold Optional manual segmentation threshold (see
#'   [segment_leaf()]).
#' @param min_object_size Optional minimum component size in pixels
#'   (overrides `config$morphology$min_object_size`).
#' @param overlay_alpha Opacity of the overlay colours.
#' @param display Logical; plot the four audit panels.
#' @param keep_images Logical; keep image arrays in the result. Set `FALSE`
#'   to save memory (masks and metrics are always kept).
#' @param config A [leaf_config()] object.
#' @return An object of class `leaf_analysis`, a list with:
#'   `original`, `cropped` (analysed region, original pixels), `normalized`
#'   (`NULL` if `normalize = FALSE`), `leaf_mask`, `healthy_mask`,
#'   `injury_mask`, `class_map`, `perforation_mask`, `overlay`, `metrics`
#'   (one-row data frame), `parameters` (see [get_analysis_parameters()]),
#'   `quality` (see [check_leaf_quality()]), `metadata`, `crop`,
#'   `segmentation`, `tissue` and `timing`.
#' @examples
#' res <- analyze_leaf(leaf_example_images()[1], crop = "auto")
#' res$metrics
#' res$quality$quality_flag
#'
#' res_mc <- analyze_leaf(leaf_example_images()[2], crop = "auto", multiclass = TRUE)
#' res_mc$metrics[, c("healthy_percent", "chlorotic_percent",
#'                    "necrotic_percent", "other_percent")]
#' @export
analyze_leaf <- function(image, method = "auto", tissue_method = "lab",
                         crop = "none", crop_points = NULL, normalize = FALSE,
                         multiclass = FALSE, threshold = NULL, min_object_size = NULL,
                         overlay_alpha = 0.45, display = FALSE, keep_images = TRUE,
                         config = leaf_config()) {
  t_start <- Sys.time()
  config <- as_leaf_config(config)
  method <- match.arg(method, leaf_methods()$method)
  tissue_method <- match.arg(tissue_method, leaf_methods()$tissue_method)
  crop <- match.arg(crop, leaf_methods()$crop)
  if (!is.null(min_object_size)) {
    config$morphology["min_object_size"] <- list(min_object_size)
    validate_config(config)
  }

  original <- resolve_image(image)
  metadata <- leaf_image_metadata(original)
  d0 <- dim(original)

  cropped <- switch(crop,
    none = original,
    auto = auto_crop_leaf(original, method = method, config = config),
    manual = {
      if (is.null(crop_points)) leaf_abort("`crop_points` is required when crop = 'manual'.")
      crop_leaf_manual(original, crop_points$x, crop_points$y)
    })
  crop_info <- attr(cropped, "crop_info") %||%
    list(method = "none", found = NA,
         bbox = c(x_min = 1, x_max = d0[1], y_min = 1, y_max = d0[2]),
         original_dim = d0[1:2])

  normalized <- if (isTRUE(normalize)) normalize_leaf_image(cropped, config = config) else NULL
  work <- normalized %||% cropped
  arr <- as_rgb_array(work)

  seg <- segment_leaf(arr, method = method, threshold = threshold, config = config)
  tissue <- classify_leaf_tissue(arr, seg$mask, method = tissue_method,
                                 multiclass = multiclass, config = config)
  counts <- if (any(seg$mask)) {
    calculate_injury(tissue$class_map, seg$mask, multiclass)
  } else {
    empty <- calculate_injury(matrix(1L, 1, 1), multiclass = multiclass)
    empty[] <- lapply(empty, function(v) if (is.integer(v)) 0L else NA_real_)
    empty$leaf_pixels <- 0L
    empty
  }
  metrics <- cbind(
    data.frame(file = metadata$filename %||% NA_character_, method = method,
               tissue_method = tissue_method, multiclass = isTRUE(multiclass),
               stringsAsFactors = FALSE),
    counts)
  quality <- compute_quality(arr, seg, metrics, config)
  metrics$perforation_candidate_pixels <- seg$holes$perforation_candidate_pixels
  metrics$quality_flag <- quality$quality_flag
  metrics$diagnostic_score <- quality$diagnostic_score
  metrics$image_width <- d0[1]
  metrics$image_height <- d0[2]
  metrics$analyzed_width <- dim(arr)[1]
  metrics$analyzed_height <- dim(arr)[2]

  overlay <- if (isTRUE(keep_images) || isTRUE(display)) {
    create_leaf_overlay(cropped, tissue$class_map, alpha = overlay_alpha,
                        multiclass = multiclass)
  } else NULL

  parameters <- build_parameters(metadata, method, tissue_method, crop, crop_info,
                                 normalize, normalized, multiclass, threshold, seg,
                                 tissue, config, overlay_alpha)
  seg_store <- seg
  seg_store$mask <- NULL
  seg_store$holes$perforation_mask <- NULL

  res <- structure(list(
    original = if (keep_images) original else NULL,
    cropped = if (keep_images) cropped else NULL,
    normalized = if (keep_images) normalized else NULL,
    leaf_mask = seg$mask,
    healthy_mask = tissue$healthy_mask,
    injury_mask = tissue$injury_mask,
    class_map = tissue$class_map,
    perforation_mask = seg$holes$perforation_mask,
    overlay = if (keep_images) overlay else NULL,
    metrics = metrics,
    parameters = parameters,
    quality = quality,
    metadata = metadata,
    crop = crop_info,
    segmentation = c(seg_store, list(mask = seg$mask)),
    tissue = list(method = tissue$method, classes = tissue$classes,
                  thresholds = tissue$thresholds, details = tissue$details),
    timing = list(elapsed_sec = as.numeric(difftime(Sys.time(), t_start, units = "secs")))
  ), class = "leaf_analysis")
  if (isTRUE(display)) {
    if (!keep_images) { res$cropped <- cropped; res$overlay <- overlay }
    graphics::plot(res)
  }
  res
}

#' @noRd
build_parameters <- function(metadata, method, tissue_method, crop, crop_info, normalize,
                             normalized, multiclass, threshold, seg, tissue, config,
                             overlay_alpha) {
  norm_info <- if (!is.null(normalized)) attr(normalized, "normalization") else NULL
  list(
    package_version = pkg_version("LeafInjuryR"),
    R_version = paste(R.version$major, R.version$minor, sep = "."),
    EBImage_version = pkg_version("EBImage"),
    analysis_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    file = metadata$filename %||% NA_character_,
    background_method = method,
    background_strategy = seg$details$strategy %||% method,
    tissue_method = tissue_method,
    multiclass = isTRUE(multiclass),
    normalization = list(enabled = isTRUE(normalize),
                         applied = norm_info$applied %||% character(0),
                         white_balance_gains = norm_info$white_balance_gains,
                         illumination_surface_range = norm_info$illumination_surface_range),
    crop_method = crop,
    crop_bbox = crop_info$bbox,
    crop_found = crop_info$found,
    thresholds = list(segmentation = seg$thresholds,
                      segmentation_source = if (is.null(threshold)) "image-derived" else "user",
                      tissue = tissue$thresholds),
    separability = seg$separability,
    segmentation_details = seg$details,
    morphological_parameters = c(config$morphology,
                                 list(min_object_size_used = seg$min_object_size_used)),
    removed_components = seg$components[!seg$components$kept, , drop = FALSE],
    hole_handling = seg$holes[setdiff(names(seg$holes), "perforation_mask")],
    background_lab = seg$background_lab,
    quality_control_parameters = config$qc,
    overlay_alpha = overlay_alpha,
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
  cat("  quality:", x$quality$quality_flag, "\n")
  for (msg in x$quality$quality_messages) cat("   -", msg, "\n")
  invisible(x)
}

#' @export
as.data.frame.leaf_analysis <- function(x, ...) x$metrics
