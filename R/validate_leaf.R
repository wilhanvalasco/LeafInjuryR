#' Validate an analysis and check its quality
#'
#' Two different questions are answered, and they are never mixed:
#'
#' 1. **Quality control** (always): heuristic indicators of the processing
#'    (leaf size, fragmentation, contrast, illumination, extreme injury
#'    values, excluded perforation candidates) and a flag `"ok"`,
#'    `"warning"` or `"failed"`. Warnings never change results.
#' 2. **Accuracy** (only when reference masks are supplied): pixel-wise
#'    agreement with manually drawn reference masks, evaluated separately for
#'    (A) leaf x background and (B) healthy x injured inside the leaf. Metrics:
#'    IoU (Jaccard), Dice, sensitivity, specificity, precision and F1, plus the
#'    error of the injury percentage. Without references **no accuracy metric
#'    is reported**.
#'
#' Reference masks must have the size of the **original** photograph (white =
#' object); cropped predictions are mapped back automatically. Use lossless
#' PNG/TIFF masks.
#'
#' @param x A `leaf_analysis` (from [analyze_leaf()]) or `leaf_batch` (from
#'   [analyze_leaf_batch()]; quality control only).
#' @param reference_leaf_mask,reference_injury_mask Optional reference masks:
#'   file paths or logical matrices `[width, height]`.
#' @param injury_region Region used for (B): `"intersection"` of reference and
#'   predicted leaf (default, isolates classification errors) or
#'   `"reference_leaf"`.
#' @return An object of class `leaf_validation` with `quality` (flag,
#'   messages, indicators, heuristic `diagnostic_score`, which is **not** a
#'   probability), `has_reference`, `metrics` (data frame or `NULL`) and
#'   `comparison` (maps for visual comparison; use `plot()`).
#' @examples
#' f <- system.file("extdata", "leaf_example_01.jpg", package = "LeafInjuryR")
#' res <- analyze_leaf(f, crop = "auto")
#' v <- validate_leaf(res)
#' v$quality$quality_flag
#' v$metrics   # NULL: no reference mask, no accuracy metrics
#'
#' # with a reference mask (here, the predicted mask itself, only to illustrate)
#' ref <- tempfile(fileext = ".png")
#' EBImage::writeImage(EBImage::Image(
#'   1 * LeafInjuryR:::uncrop_mask(res$leaf_mask, res$crop)), ref)
#' validate_leaf(res, reference_leaf_mask = ref)$metrics
#' @export
validate_leaf <- function(x, reference_leaf_mask = NULL, reference_injury_mask = NULL,
                          injury_region = c("intersection", "reference_leaf")) {
  injury_region <- match.arg(injury_region)
  if (inherits(x, "leaf_batch")) {
    if (!is.null(reference_leaf_mask) || !is.null(reference_injury_mask)) {
      leaf_abort("Reference masks are evaluated per image: use validate_leaf() on a single analysis.")
    }
    r <- x$results
    qt <- r[intersect(c("file", "status", "quality_flag", "quality_messages", "error_message",
                        "diagnostic_score"), names(r))]
    return(structure(list(quality = qt, has_reference = FALSE, metrics = NULL, comparison = NULL,
                          note = "Quality control only; accuracy requires reference masks."),
                     class = "leaf_validation"))
  }
  if (!inherits(x, "leaf_analysis")) {
    leaf_abort("`x` must be a 'leaf_analysis' or 'leaf_batch' object.")
  }
  quality <- x$quality
  has_ref <- !is.null(reference_leaf_mask) || !is.null(reference_injury_mask)
  metrics <- NULL; comparison <- NULL
  if (has_ref) {
    od <- x$crop$original_dim
    load <- function(m) {
      if (is.null(m)) return(NULL)
      if (is.character(m)) m <- read_reference_mask(m, target_dim = od)
      check_mask(m, od, "reference mask")
    }
    ref_leaf <- load(reference_leaf_mask)
    ref_inj <- load(reference_injury_mask)
    metrics <- evaluate_leaf_analysis(x, ref_leaf, ref_inj, injury_region = injury_region)
    pred_leaf <- uncrop_mask(x$leaf_mask, x$crop)
    pred_inj <- uncrop_mask(x$injury_mask, x$crop)
    code <- function(pred, ref, region = NULL) {
      m <- matrix(0L, nrow(pred), ncol(pred))
      m[pred & ref] <- 1L; m[pred & !ref] <- 2L; m[!pred & ref] <- 3L
      if (!is.null(region)) m[!region] <- 0L
      m
    }
    comparison <- list(
      leaf = if (!is.null(ref_leaf)) code(pred_leaf, ref_leaf) else NULL,
      injury = if (!is.null(ref_inj)) code(pred_inj, ref_inj,
                                           if (is.null(ref_leaf)) pred_leaf else ref_leaf | pred_leaf) else NULL,
      image = x$original)
  }
  structure(list(quality = quality, has_reference = has_ref, metrics = metrics,
                 comparison = comparison,
                 note = if (has_ref) "Accuracy computed against the supplied reference masks."
                 else "No reference mask supplied: quality-control indicators only, no accuracy metrics."),
            class = "leaf_validation")
}

#' @export
print.leaf_validation <- function(x, ...) {
  cat("<leaf_validation>\n")
  if (is.data.frame(x$quality)) {
    cat("  batch quality flags:\n"); print(table(x$quality$quality_flag))
  } else {
    cat("  quality:", x$quality$quality_flag, "\n")
    for (m in x$quality$quality_messages) cat("   -", m, "\n")
  }
  if (!is.null(x$metrics)) {
    cat("  accuracy against reference masks:\n")
    print(x$metrics[intersect(c("label", "iou", "dice", "sensitivity", "specificity", "precision",
                                "f1", "injured_percent_error"), names(x$metrics))], row.names = FALSE)
  }
  cat(" ", x$note, "\n")
  invisible(x)
}

#' @export
plot.leaf_validation <- function(x, ...) {
  if (is.null(x$comparison)) {
    graphics::plot.new(); graphics::text(0.5, 0.5, x$note); return(invisible(x))
  }
  maps <- Filter(Negate(is.null), x$comparison[c("leaf", "injury")])
  op <- graphics::par(mfrow = c(1, length(maps)), oma = c(2, 0, 0, 0)); on.exit(graphics::par(op))
  cols <- validation_colors()
  for (nm in names(maps)) {
    plot_leaf_image(comparison_overlay(x$comparison$image, maps[[nm]]),
                    main = sprintf("%s: agreement with reference", if (nm == "leaf") "Leaf" else "Injury"))
  }
  graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  graphics::plot.new()
  graphics::legend("bottom", horiz = TRUE, bty = "n", fill = cols,
                   legend = c("Agree (TP)", "False positive", "False negative"))
  invisible(x)
}

#' @noRd
validation_colors <- function() c("#2E9D5B", "#E4572E", "#2F6DB5")

#' @noRd
comparison_overlay <- function(image, code, alpha = 0.55) {
  arr <- as_rgb_array(image)
  rgbm <- grDevices::col2rgb(validation_colors()) / 255
  for (k in 1:3) {
    sel <- which(code == k)
    if (!length(sel)) next
    for (ch in 1:3) { l <- arr[, , ch]; l[sel] <- (1 - alpha) * l[sel] + alpha * rgbm[ch, k]; arr[, , ch] <- l }
  }
  as_color_image(arr)
}
