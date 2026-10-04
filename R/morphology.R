#' Clean a raw leaf mask (morphology, connected components, holes)
#'
#' Applies, in order: (1) morphological opening; (2) connected-component
#' labelling and area computation; (3) removal of components smaller than the
#' minimum object size; (4) optional retention of the largest component only
#' (main leaf); (5) hole handling according to `hole_policy`. Every removed
#' component is recorded in the returned `components` table together with the
#' reason, so no potentially valid object is discarded silently.
#'
#' Hole handling does **not** assume that gaps inside the leaf outline are
#' injury. With `hole_policy = "fill_non_background"` a hole pixel is filled
#' only if its colour differs from the estimated background colour by at least
#' `background_delta_e` (CIE76). Background-coloured holes larger than
#' `min_perforation_size` are left out of the leaf mask and reported as
#' *perforation candidates* (experimental; they may be true perforations,
#' tissue loss, glare or segmentation artefacts). They are never counted as
#' injured pixels.
#'
#' @param mask Logical matrix `[width, height]` (raw foreground).
#' @param image Image or RGB array used for colour-based hole handling.
#'   Required for `hole_policy = "fill_non_background"`.
#' @param background_lab Numeric vector `c(L, a, b)` of the background colour.
#'   If `NULL` it is estimated from pixels outside `mask` on the image border.
#' @param config A [leaf_config()] object.
#' @return A list with `mask` (clean logical matrix), `components` (data frame:
#'   `component`, `area`, `kept`, `reason`), `n_raw_components`,
#'   `main_component_share` and `holes` (list with pixel counts, the
#'   perforation-candidate mask and the policy used).
#' @noRd
clean_leaf_mask <- function(mask, image = NULL, background_lab = NULL,
                            config = leaf_config()) {
  config <- as_leaf_config(config)
  mc <- config$morphology
  mask <- check_mask(mask)
  d <- dim(mask)
  n_pix <- length(mask)

  if (mc$opening_radius > 0) {
    mask <- EBImage::opening(mask * 1, make_brush(mc$opening_radius, mc$brush_shape)) > 0
  }

  labels <- EBImage::bwlabel(mask * 1)
  labels <- matrix(as.integer(labels), d[1], d[2])
  n_comp <- max(labels)
  areas <- if (n_comp > 0) tabulate(labels[labels > 0], nbins = n_comp) else integer(0)
  min_size <- mc$min_object_size %||% ceiling(mc$min_object_fraction * n_pix)

  comp <- data.frame(component = seq_len(n_comp), area = areas,
                     kept = rep(TRUE, n_comp), reason = rep("kept", n_comp),
                     stringsAsFactors = FALSE)
  if (n_comp > 0) {
    small <- comp$area < min_size
    comp$kept[small] <- FALSE
    comp$reason[small] <- "below_min_object_size"
    if (isTRUE(mc$keep_largest) && any(comp$kept)) {
      largest <- which.max(ifelse(comp$kept, comp$area, -1))
      others <- comp$kept & comp$component != largest
      comp$kept[others] <- FALSE
      comp$reason[others] <- "not_largest_component"
    }
    comp <- comp[order(-comp$area), , drop = FALSE]
    rownames(comp) <- NULL
  }
  keep_ids <- comp$component[comp$kept]
  mask <- labels %in% keep_ids
  dim(mask) <- d
  main_share <- if (sum(areas) > 0) max(areas) / sum(areas) else NA_real_

  holes <- list(policy = if (isTRUE(mc$fill_holes)) mc$hole_policy else "none",
                hole_pixels = 0L, filled_pixels = 0L,
                perforation_candidate_pixels = 0L,
                perforation_mask = matrix(FALSE, d[1], d[2]))
  if (isTRUE(mc$fill_holes) && mc$hole_policy != "none" && any(mask)) {
    filled <- EBImage::fillHull(mask * 1) > 0
    hole <- filled & !mask
    holes$hole_pixels <- sum(hole)
    if (holes$hole_pixels > 0) {
      if (mc$hole_policy == "fill_all") {
        mask <- filled
        holes$filled_pixels <- holes$hole_pixels
      } else {
        if (is.null(image)) {
          leaf_abort("`image` is required for hole_policy = 'fill_non_background'.")
        }
        arr <- as_rgb_array(image)
        if (is.null(background_lab)) {
          background_lab <- estimate_background_lab(arr, !filled, config)
        }
        idx <- which(hole)
        lab <- rgb_to_lab(arr[, , 1][idx], arr[, , 2][idx], arr[, , 3][idx])
        de <- sqrt((lab$L - background_lab[1])^2 + (lab$a - background_lab[2])^2 +
                     (lab$b - background_lab[3])^2)
        bg_like <- matrix(FALSE, d[1], d[2])
        bg_like[idx[de < mc$background_delta_e]] <- TRUE
        perf <- matrix(FALSE, d[1], d[2])
        if (any(bg_like)) {
          pl <- matrix(as.integer(EBImage::bwlabel(bg_like * 1)), d[1], d[2])
          pa <- tabulate(pl[pl > 0], nbins = max(pl))
          big <- which(pa >= mc$min_perforation_size)
          perf <- pl %in% big
          dim(perf) <- d
        }
        fill_px <- hole & !perf
        mask <- mask | fill_px
        holes$filled_pixels <- sum(fill_px)
        holes$perforation_candidate_pixels <- sum(perf)
        holes$perforation_mask <- perf
      }
    }
  }
  list(mask = mask, components = comp, n_raw_components = n_comp,
       main_component_share = main_share, min_object_size_used = min_size,
       holes = holes)
}

#' Estimate background colour (median L*a*b*) from background border pixels
#' @noRd
estimate_background_lab <- function(arr, background, config) {
  d <- dim(arr)
  sel <- background & border_mask(d, config$segmentation$border_fraction)
  if (sum(sel) < 10) sel <- background
  if (sum(sel) < 10) return(c(NA_real_, NA_real_, NA_real_))
  idx <- which(sel)
  lab <- rgb_to_lab(arr[, , 1][idx], arr[, , 2][idx], arr[, , 3][idx])
  c(L = stats::median(lab$L), a = stats::median(lab$a), b = stats::median(lab$b))
}

#' Remove small connected regions from a logical mask
#' @noRd
remove_small_regions <- function(mask, min_size) {
  if (min_size <= 0 || !any(mask)) return(mask)
  d <- dim(mask)
  lab <- matrix(as.integer(EBImage::bwlabel(mask * 1)), d[1], d[2])
  a <- tabulate(lab[lab > 0], nbins = max(lab))
  out <- lab %in% which(a >= min_size)
  dim(out) <- d
  out
}
