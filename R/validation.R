#' Compare a predicted mask with a reference (ground-truth) mask
#'
#' Computes pixel-wise agreement between an automatic mask and a manually
#' produced reference mask. Use it separately for:
#' * **A) leaf x background**: `predicted = result$leaf_mask`,
#'   `reference = reference_leaf_mask`;
#' * **B) healthy x injured**: `predicted = result$injury_mask`,
#'   `reference = reference_injury_mask`, restricted with `region` to the
#'   reference leaf (so background does not inflate specificity).
#'
#' Metrics (TP, FP, FN, TN pixel counts; positive = `TRUE`):
#' IoU/Jaccard = TP/(TP+FP+FN); Dice = 2TP/(2TP+FP+FN); accuracy =
#' (TP+TN)/N; sensitivity/recall = TP/(TP+FN); specificity = TN/(TN+FP);
#' precision = TP/(TP+FP); F1 = 2 precision recall/(precision+recall) (equal
#' to Dice for binary masks). Undefined ratios are `NA`.
#'
#' Performance can only be claimed against manual references; visual
#' plausibility of a mask is not evidence of accuracy.
#'
#' @param predicted_mask,reference_mask Logical matrices of equal size (or
#'   images/paths readable by [read_reference_mask()]).
#' @param region Optional logical matrix restricting evaluation.
#' @param label Optional label stored in the output (e.g. `"leaf"`).
#' @return A one-row data frame with counts and metrics.
#' @examples
#' ref <- matrix(FALSE, 10, 10); ref[3:8, 3:8] <- TRUE
#' pred <- matrix(FALSE, 10, 10); pred[4:8, 3:8] <- TRUE
#' evaluate_segmentation(pred, ref)
#' @export
evaluate_segmentation <- function(predicted_mask, reference_mask, region = NULL,
                                  label = NA_character_) {
  if (is.character(reference_mask)) reference_mask <- read_reference_mask(reference_mask)
  if (is.character(predicted_mask)) predicted_mask <- read_reference_mask(predicted_mask)
  ref <- check_mask(reference_mask, name = "reference_mask")
  pred <- check_mask(predicted_mask, dim(ref), "predicted_mask")
  if (!is.null(region)) {
    region <- check_mask(region, dim(ref), "region")
    ref <- ref[region]; pred <- pred[region]
  }
  tp <- sum(pred & ref); fp <- sum(pred & !ref)
  fn <- sum(!pred & ref); tn <- sum(!pred & !ref)
  ratio <- function(a, b) if (b > 0) a / b else NA_real_
  precision <- ratio(tp, tp + fp)
  recall <- ratio(tp, tp + fn)
  data.frame(
    label = label, n_pixels = tp + fp + fn + tn,
    tp = tp, fp = fp, fn = fn, tn = tn,
    iou = ratio(tp, tp + fp + fn),
    dice = ratio(2 * tp, 2 * tp + fp + fn),
    accuracy = ratio(tp + tn, tp + fp + fn + tn),
    sensitivity = recall,
    specificity = ratio(tn, tn + fp),
    precision = precision,
    f1 = if (!is.na(precision) && !is.na(recall) && precision + recall > 0)
      2 * precision * recall / (precision + recall) else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Evaluate a leaf analysis against reference masks
#'
#' Maps the predicted masks back to the original image size (undoing any crop)
#' and evaluates (A) leaf segmentation and (B) injury classification. For (B)
#' the evaluation region is the intersection of reference and predicted leaf
#' masks by default, so that segmentation and classification errors are
#' assessed separately; the injured-percent error is also reported.
#'
#' @param result A `leaf_analysis` object.
#' @param reference_leaf_mask Reference leaf mask (matrix or path), full
#'   original image size. May be `NULL`.
#' @param reference_injury_mask Reference injury mask (matrix or path), full
#'   original image size. May be `NULL`.
#' @param injury_region `"intersection"` (default) or `"reference_leaf"`.
#' @return A data frame with one row per evaluated task (`leaf`, `injury`)
#'   plus columns `reference_injured_percent`, `predicted_injured_percent`
#'   and `injured_percent_error` on the injury row.
#' @examples
#' syn <- make_synthetic_leaf()
#' res <- analyze_leaf(syn$image)
#' evaluate_leaf_analysis(res, syn$leaf_mask, syn$injury_mask)
#' @export
evaluate_leaf_analysis <- function(result, reference_leaf_mask = NULL,
                                   reference_injury_mask = NULL,
                                   injury_region = c("intersection", "reference_leaf")) {
  if (!inherits(result, "leaf_analysis")) leaf_abort("`result` must be a 'leaf_analysis'.")
  injury_region <- match.arg(injury_region)
  od <- result$crop$original_dim
  pred_leaf <- uncrop_mask(result$leaf_mask, result$crop)
  pred_inj <- uncrop_mask(result$injury_mask, result$crop)
  load <- function(m) {
    if (is.null(m)) return(NULL)
    if (is.character(m)) m <- read_reference_mask(m, target_dim = od)
    check_mask(m, od, "reference mask")
  }
  ref_leaf <- load(reference_leaf_mask)
  ref_inj <- load(reference_injury_mask)
  out <- list()
  if (!is.null(ref_leaf)) out$leaf <- evaluate_segmentation(pred_leaf, ref_leaf, label = "leaf")
  if (!is.null(ref_inj)) {
    region <- if (is.null(ref_leaf)) pred_leaf else if (injury_region == "intersection")
      ref_leaf & pred_leaf else ref_leaf
    ev <- evaluate_segmentation(pred_inj, ref_inj & region, region = region, label = "injury")
    ref_den <- if (is.null(ref_leaf)) sum(pred_leaf) else sum(ref_leaf)
    ev$reference_injured_percent <- safe_percent(sum(ref_inj & (ref_leaf %||% pred_leaf)), ref_den)
    ev$predicted_injured_percent <- result$metrics$injured_percent
    ev$injured_percent_error <- ev$predicted_injured_percent - ev$reference_injured_percent
    out$injury <- ev
  }
  if (!length(out)) leaf_abort("Supply at least one reference mask.")
  cols <- unique(unlist(lapply(out, names)))
  out <- lapply(out, function(df) { for (cc in setdiff(cols, names(df))) df[[cc]] <- NA_real_; df[cols] })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

#' Read a manually produced reference mask
#'
#' Reads a PNG/TIFF/JPEG mask in which the object (leaf or injured tissue)
#' is white/non-zero and everything else black. Colour masks are reduced to
#' their maximum channel. Lossless formats (PNG, TIFF) are strongly
#' recommended; JPEG compression alters mask edges.
#'
#' @param path Mask file.
#' @param threshold Grey value (0-1) above which a pixel is `TRUE`.
#' @param target_dim Optional expected `c(width, height)`; an error is raised
#'   if the mask size differs (masks are never silently resized).
#' @return Logical matrix `[width, height]`.
#' @examples
#' f <- tempfile(fileext = ".png")
#' write_mask_png(make_synthetic_leaf()$leaf_mask, f)
#' m <- read_reference_mask(f)
#' @export
read_reference_mask <- function(path, threshold = 0.5, target_dim = NULL) {
  if (!file.exists(path)) leaf_abort(sprintf("Reference mask not found: '%s'.", path))
  img <- tryCatch(EBImage::readImage(path), error = function(e) NULL)
  if (is.null(img)) leaf_abort("Reference mask file could not be read.")
  a <- EBImage::imageData(img)
  m <- if (length(dim(a)) == 3L) apply(a[, , seq_len(min(3, dim(a)[3])), drop = FALSE], c(1, 2), max) else a
  m <- m > threshold
  if (!is.null(target_dim) && !identical(as.integer(dim(m)), as.integer(target_dim[1:2]))) {
    leaf_abort(sprintf("Reference mask size (%s) differs from image size (%s).",
                       paste(dim(m), collapse = " x "), paste(target_dim[1:2], collapse = " x ")))
  }
  m
}

#' Build a ground-truth manifest
#'
#' A ground-truth set links each image to its manually produced reference
#' masks. This package does **not** create artificial ground truth: the
#' example images shipped with the package have no reference masks and are
#' functional examples only.
#'
#' @param image Character vector of image paths.
#' @param reference_leaf_mask Character vector of leaf-mask paths (`NA` if
#'   unavailable).
#' @param reference_injury_mask Character vector of injury-mask paths (`NA`
#'   if unavailable).
#' @return A data frame with columns `image`, `reference_leaf_mask`,
#'   `reference_injury_mask`.
#' @seealso [read_ground_truth()], [validate_ground_truth()]
#' @examples
#' ground_truth_manifest("leaf01.jpg", "leaf01_leaf.png", "leaf01_injury.png")
#' @export
ground_truth_manifest <- function(image, reference_leaf_mask = NA_character_,
                                  reference_injury_mask = NA_character_) {
  n <- length(image)
  rep_to <- function(v) if (length(v) == 1L) rep(v, n) else v
  df <- data.frame(image = as.character(image),
                   reference_leaf_mask = as.character(rep_to(reference_leaf_mask)),
                   reference_injury_mask = as.character(rep_to(reference_injury_mask)),
                   stringsAsFactors = FALSE)
  if (nrow(df) != n) leaf_abort("All arguments must have the same length.")
  df
}

#' Read a ground-truth manifest from CSV
#'
#' The CSV must contain the columns `image`, `reference_leaf_mask` and
#' `reference_injury_mask` (empty cells allowed). Relative paths are resolved
#' against the CSV location. A template is shipped at
#' `system.file("extdata", "ground_truth_template.csv", package = "LeafInjuryR")`.
#'
#' @param file CSV file path.
#' @return A manifest data frame (see [ground_truth_manifest()]).
#' @examples
#' tpl <- system.file("extdata", "ground_truth_template.csv", package = "LeafInjuryR")
#' read.csv(tpl)
#' @export
read_ground_truth <- function(file) {
  df <- utils::read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  need <- c("image", "reference_leaf_mask", "reference_injury_mask")
  miss <- setdiff(need, names(df))
  if (length(miss)) leaf_abort(sprintf("Ground-truth CSV lacks column(s): %s.", paste(miss, collapse = ", ")))
  base <- dirname(normalizePath(file))
  fix <- function(p) ifelse(is.na(p) | grepl("^(/|[A-Za-z]:)", p), p, file.path(base, p))
  for (cc in need) df[[cc]] <- fix(df[[cc]])
  df[need]
}

#' Validate the pipeline against a ground-truth set
#'
#' Runs [analyze_leaf()] on every image of the manifest (with the supplied
#' settings) and compares the results with the reference masks using
#' [evaluate_leaf_analysis()]. Summary statistics across images should be
#' reported together with the number of images and the imaging conditions.
#'
#' @param manifest Data frame from [ground_truth_manifest()] or
#'   [read_ground_truth()].
#' @param ... Arguments passed to [analyze_leaf()].
#' @return A data frame with one row per image and task.
#' @examples
#' dir <- tempfile(); dir.create(dir)
#' syn <- make_synthetic_leaf()
#' write_image_png(syn$image, file.path(dir, "s.png"))
#' write_mask_png(syn$leaf_mask, file.path(dir, "s_leaf.png"))
#' write_mask_png(syn$injury_mask, file.path(dir, "s_injury.png"))
#' man <- ground_truth_manifest(file.path(dir, "s.png"), file.path(dir, "s_leaf.png"),
#'                              file.path(dir, "s_injury.png"))
#' validate_ground_truth(man)
#' @export
validate_ground_truth <- function(manifest, ...) {
  rows <- lapply(seq_len(nrow(manifest)), function(i) {
    res <- analyze_leaf(manifest$image[i], keep_images = FALSE, ...)
    lm <- manifest$reference_leaf_mask[i]; im <- manifest$reference_injury_mask[i]
    ev <- evaluate_leaf_analysis(res, if (is.na(lm)) NULL else lm,
                                 if (is.na(im)) NULL else im)
    cbind(image = basename(manifest$image[i]), ev, stringsAsFactors = FALSE)
  })
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(df) { for (cc in setdiff(cols, names(df))) df[[cc]] <- NA; df[cols] })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
