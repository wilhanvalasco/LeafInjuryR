#' Quantify injury inside the leaf
#'
#' Counts pixels per class **inside the leaf mask only** and expresses them as
#' percentages of leaf pixels (the denominator is always `leaf_pixels`, never
#' the image area). Any class code found outside the leaf mask is ignored.
#'
#' Binary mode: `healthy_pixels + injured_pixels = leaf_pixels`.
#' Multiclass mode additionally reports `chlorotic`, `necrotic` and `other`,
#' with `injured = chlorotic + necrotic + other`.
#'
#' Pixels excluded from the leaf mask (e.g. background-coloured perforation
#' candidates, see [clean_leaf_mask()]) are **not** counted as injury.
#'
#' @param class_map Integer class map (see [leaf_classes()]) or a
#'   `leaf_tissue` object.
#' @param leaf_mask Logical leaf mask. If `NULL`, `class_map > 0` is used.
#' @param multiclass Logical; if `NULL`, taken from the `leaf_tissue` object
#'   or inferred from the class codes.
#' @return A one-row data frame with `leaf_pixels`, `healthy_pixels`,
#'   `injured_pixels`, `healthy_percent`, `injured_percent` and, in multiclass
#'   mode, `chlorotic_pixels`, `necrotic_pixels`, `other_pixels` and the
#'   corresponding percentages. Percentages are not rounded.
#' @noRd
calculate_injury <- function(class_map, leaf_mask = NULL, multiclass = NULL) {
  if (inherits(class_map, "leaf_tissue")) {
    multiclass <- multiclass %||% class_map$multiclass
    class_map <- class_map$class_map
  }
  if (!is.matrix(class_map)) leaf_abort("`class_map` must be an integer matrix.")
  leaf_mask <- if (is.null(leaf_mask)) class_map > 0 else check_mask(leaf_mask, dim(class_map), "leaf_mask")
  multiclass <- multiclass %||% any(class_map[leaf_mask] > 2L)
  inside <- class_map[leaf_mask]
  leaf_px <- length(inside)
  counts <- tabulate(inside, nbins = 4L)  # codes 1..4; 0 inside leaf is unclassified
  healthy <- counts[1]
  injured <- sum(counts[2:4])
  out <- data.frame(
    leaf_pixels = leaf_px,
    healthy_pixels = healthy,
    injured_pixels = injured,
    healthy_percent = safe_percent(healthy, leaf_px),
    injured_percent = safe_percent(injured, leaf_px)
  )
  unclassified <- sum(inside == 0L)
  if (unclassified > 0) {
    leaf_warn(sprintf("%d leaf pixel(s) have no class (code 0) and are counted in neither class.",
                      unclassified))
  }
  if (isTRUE(multiclass)) {
    out$chlorotic_pixels <- counts[2]
    out$necrotic_pixels <- counts[3]
    out$other_pixels <- counts[4]
    out$chlorotic_percent <- safe_percent(counts[2], leaf_px)
    out$necrotic_percent <- safe_percent(counts[3], leaf_px)
    out$other_percent <- safe_percent(counts[4], leaf_px)
  }
  out
}
