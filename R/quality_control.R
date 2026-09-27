#' Quality control of a leaf analysis
#'
#' Evaluates heuristic indicators and returns a flag:
#' * `"ok"`: no issue detected;
#' * `"warning"`: the result was computed but should be inspected;
#' * `"failed"`: no valid result (e.g. empty leaf mask).
#'
#' Warnings **never change the result**; they accompany it. Limits are in
#' `config$qc` (see [leaf_config()]).
#'
#' `diagnostic_score` (0-1) is a *heuristic* summary of segmentation contrast,
#' mask compactness (share of the largest component) and background
#' illumination homogeneity. It is **not** a probability or a calibrated
#' confidence measure, and it says nothing about the correctness of tissue
#' classification.
#'
#' @param x A `leaf_analysis` object from [analyze_leaf()].
#' @param config A [leaf_config()] object; defaults to the configuration
#'   stored in `x`.
#' @return A list with `quality_flag`, `quality_messages` (character),
#'   `indicators` (named list) and `diagnostic_score`.
#' @examples
#' res <- analyze_leaf(leaf_example_images()[1])
#' check_leaf_quality(res)$quality_flag
#' @export
check_leaf_quality <- function(x, config = NULL) {
  if (!inherits(x, "leaf_analysis")) {
    leaf_abort("`x` must be a 'leaf_analysis' object returned by analyze_leaf().")
  }
  config <- config %||% x$parameters$config
  img <- x$normalized %||% x$cropped %||% x$original
  compute_quality(as_rgb_array(img), x$segmentation, x$metrics, config)
}

#' @noRd
compute_quality <- function(arr, seg, metrics, config) {
  qc <- config$qc
  warn <- character(0); fail <- character(0)
  d <- dim(arr)
  n_pix <- d[1] * d[2]
  leaf_px <- sum(seg$mask)
  ind <- list(leaf_fraction = leaf_px / n_pix,
              n_raw_components = seg$n_raw_components,
              main_component_share = seg$main_component_share,
              contrast = NA_real_, illumination_cv = NA_real_,
              perforation_candidate_pixels = seg$holes$perforation_candidate_pixels)

  if (!any(seg$raw_mask)) fail <- c(fail, "No leaf object was detected.")
  if (leaf_px == 0) fail <- c(fail, "Leaf mask is empty.")

  seps <- unlist(seg$separability)
  used <- names(seg$thresholds)
  ind$contrast <- if (length(used) && all(used %in% names(seps))) {
    max(seps[used])
  } else if (length(seps)) max(seps) else NA_real_

  if (!length(fail)) {
    if (ind$leaf_fraction < qc$min_leaf_fraction)
      warn <- c(warn, sprintf("Leaf is very small (%.1f%% of the image).", 100 * ind$leaf_fraction))
    if (ind$leaf_fraction > qc$max_leaf_fraction)
      warn <- c(warn, sprintf("Leaf occupies almost the whole image (%.1f%%); background may be insufficient.",
                              100 * ind$leaf_fraction))
    if (!is.na(ind$n_raw_components) && ind$n_raw_components > qc$max_raw_components)
      warn <- c(warn, sprintf("Large number of disconnected components (%d) before cleaning.",
                              ind$n_raw_components))
    if (!is.na(ind$main_component_share) && ind$main_component_share < qc$min_main_component_share)
      warn <- c(warn, sprintf("Fragmented mask: largest component holds %.1f%% of foreground.",
                              100 * ind$main_component_share))
    if (!is.na(ind$contrast) && ind$contrast < qc$min_contrast)
      warn <- c(warn, sprintf("Low leaf/background contrast (separability %.2f).", ind$contrast))
    if (isTRUE(ind$perforation_candidate_pixels > 0))
      warn <- c(warn, sprintf("%d background-coloured pixels inside the leaf outline were excluded (perforation candidates, experimental).",
                              ind$perforation_candidate_pixels))
    ind$illumination_cv <- illumination_cv(arr, seg$raw_mask | seg$mask, qc$illumination_grid)
    if (!is.na(ind$illumination_cv) && ind$illumination_cv > qc$max_illumination_cv)
      warn <- c(warn, sprintf("Heterogeneous background illumination (CV %.3f).", ind$illumination_cv))
    ip <- metrics$injured_percent
    if (!is.null(ip) && !is.na(ip)) {
      if (ip < qc$near_zero_injury)
        warn <- c(warn, sprintf("Injury close to 0%% (%.2f%%); check classification.", ip))
      if (ip > qc$near_full_injury)
        warn <- c(warn, sprintf("Injury close to 100%% (%.2f%%); check segmentation/classification.", ip))
    }
    op <- metrics$other_percent
    if (!is.null(op) && !is.na(op) && op > qc$max_other_percent)
      warn <- c(warn, sprintf("'Other' class covers %.1f%% of the leaf (glare, residues or unusual colours?).", op))
  }
  flag <- if (length(fail)) "failed" else if (length(warn)) "warning" else "ok"
  score <- if (length(fail)) 0 else {
    parts <- c(min(1, ind$contrast %||% NA),
               ind$main_component_share,
               if (is.na(ind$illumination_cv)) NA else
                 max(0, 1 - ind$illumination_cv / (2 * qc$max_illumination_cv)))
    mean(parts, na.rm = TRUE)
  }
  list(quality_flag = flag, quality_messages = c(fail, warn), indicators = ind,
       diagnostic_score = score,
       diagnostic_score_note = "Heuristic indicator (0-1); not a probability or calibrated confidence.")
}

#' Coefficient of variation of background lightness across grid tiles
#' @noRd
illumination_cv <- function(arr, foreground, grid) {
  d <- dim(arr)
  L <- rgb_to_lab(arr[, , 1], arr[, , 2], arr[, , 3])$L
  bg <- !foreground
  xi <- pmin(grid, ceiling(row(L) / d[1] * grid))
  yi <- pmin(grid, ceiling(col(L) / d[2] * grid))
  tile <- (yi - 1L) * grid + xi
  med <- vapply(seq_len(grid * grid), function(k) {
    sel <- bg & tile == k
    if (sum(sel) < 50) NA_real_ else stats::median(L[sel])
  }, numeric(1))
  med <- med[!is.na(med)]
  if (length(med) < 3 || mean(med) <= 0) return(NA_real_)
  stats::sd(med) / mean(med)
}
