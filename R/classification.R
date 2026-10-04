#' Class codes used in class maps
#'
#' Class maps are integer matrices. `0` is always background (outside the leaf
#' mask). Binary mode: `1` healthy, `2` injured. Multiclass mode: `1` healthy,
#' `2` chlorotic, `3` necrotic, `4` other.
#'
#' @param multiclass Logical.
#' @return Named integer vector of class codes.
#' @noRd
leaf_classes <- function(multiclass = FALSE) {
  if (isTRUE(multiclass)) {
    c(background = 0L, healthy = 1L, chlorotic = 2L, necrotic = 3L, other = 4L)
  } else {
    c(background = 0L, healthy = 1L, injured = 2L)
  }
}

#' Classify leaf tissue inside the leaf mask
#'
#' Classifies **only pixels inside `leaf_mask`**; pixels outside the mask are
#' background (class 0) and can never contribute to healthy or injured counts.
#' Always run [segment_leaf_mask()] first.
#'
#' The classification is done in two stages, which guarantees that the binary
#' result and the multiclass result are consistent:
#' 1. The selected `method` decides *healthy* vs *not healthy*.
#' 2. If `multiclass = TRUE`, not-healthy pixels are subdivided with CIELAB
#'    rules into *chlorotic*, *necrotic* and *other*. Healthy pixels are not
#'    changed. Hence `injured = chlorotic + necrotic + other` exactly.
#'
#' Achromatic pixels (Lab chroma < `achromatic_chroma_max`) are never healthy.
#'
#' **Why the default is `"lab"` (fixed thresholds) and not `"auto"`.** When
#' images from different treatments are compared, a threshold that adapts to
#' each image can change *with the injury itself* (a within-leaf Otsu split
#' of a heavily injured leaf differs from that of a lightly injured one),
#' which may bias comparisons. Fixed, documented thresholds are applied
#' identically to all images. The fixed defaults are starting values and must
#' be calibrated/validated for each imaging setup (see
#' `vignette("method-validation")`).
#'
#' @section Methods (stage 1):
#' \describe{
#'   \item{`"lab"`}{Healthy if Lab hue angle is within
#'     \[`lab_healthy_hue_min`, `lab_healthy_hue_max`\].}
#'   \item{`"hsv"`}{Healthy if HSV hue is within
#'     \[`hsv_healthy_hue_min`, `hsv_healthy_hue_max`\] and saturation >=
#'     `hsv_min_saturation`.}
#'   \item{`"exgr"`}{Healthy if ExG - ExR > `exgr_threshold`. Mostly detects
#'     necrosis; yellowish (chlorotic) tissue is usually classed healthy.}
#'   \item{`"vote"`}{Healthy if at least two of `lab`, `hsv`, `exgr` agree.}
#'   \item{`"auto"`}{Like `"lab"`, but the lower hue limit is the Otsu
#'     threshold of Lab hue computed inside this leaf, constrained to
#'     `auto_hue_bounds`. If the within-leaf hue distribution is not bimodal
#'     the constraint prevents an arbitrary split. The value used is stored.}
#' }
#'
#' @section Multiclass rules (stage 2, not-healthy pixels only):
#' \describe{
#'   \item{necrotic}{Achromatic and dark (L* <= `dark_lightness_max`), or Lab
#'     hue < `lab_necrotic_hue_max`, or hue >= 300.}
#'   \item{chlorotic}{Chromatic, hue in \[`lab_necrotic_hue_max`, healthy
#'     lower limit).}
#'   \item{other}{Everything else (e.g. bright achromatic pixels such as glare
#'     or residues, bluish tones, or pixels judged not healthy by stage 1 but
#'     with a green Lab hue).}
#' }
#' These colour classes describe visual patterns. Similar colours can have
#' different biological causes; the classes are **not** diagnoses.
#'
#' @param image Image or path (the same image used for segmentation).
#' @param leaf_mask Logical matrix from [segment_leaf_mask()].
#' @param method Tissue classification method (see *Methods*).
#' @param multiclass Logical; subdivide injured tissue.
#' @param config A [leaf_config()] object.
#' @return An object of class `leaf_tissue`: list with `class_map`,
#'   `classes`, `healthy_mask`, `injury_mask`, `method`, `multiclass`,
#'   `thresholds`, `details`.
#' @noRd
classify_leaf_tissue <- function(image, leaf_mask, method = "lab", multiclass = FALSE,
                                 config = leaf_config()) {
  config <- as_leaf_config(config)
  tc <- config$tissue
  method <- match.arg(method, leaf_methods()$tissue_method)
  arr <- as_rgb_array(resolve_image(image))
  d <- dim(arr)
  leaf_mask <- check_mask(leaf_mask, d, "leaf_mask")
  classes <- leaf_classes(multiclass)
  class_map <- matrix(0L, d[1], d[2])
  thresholds <- list(); details <- list()

  idx <- which(leaf_mask)
  if (length(idx) == 0L) {
    return(structure(list(class_map = class_map, classes = classes,
                          healthy_mask = leaf_mask, injury_mask = leaf_mask,
                          method = method, multiclass = multiclass,
                          thresholds = thresholds,
                          details = list(note = "empty leaf mask")),
                     class = "leaf_tissue"))
  }
  r <- arr[, , 1][idx]; g <- arr[, , 2][idx]; b <- arr[, , 3][idx]
  lab <- rgb_to_lab(r, g, b)
  achromatic <- lab$chroma < tc$achromatic_chroma_max

  lab_rule <- function(lower) {
    !achromatic & lab$hue >= lower & lab$hue <= tc$lab_healthy_hue_max
  }
  hsv_rule <- function() {
    h <- rgb_to_hsv(r, g, b)
    !achromatic & h$saturation >= tc$hsv_min_saturation &
      h$hue >= tc$hsv_healthy_hue_min & h$hue <= tc$hsv_healthy_hue_max
  }
  exgr_rule <- function() {
    !achromatic & vegetation_indices(r, g, b)$exgr > tc$exgr_threshold
  }
  healthy_lower <- tc$lab_healthy_hue_min
  healthy <- switch(method,
    lab = {
      thresholds$lab_healthy_hue_min <- healthy_lower
      lab_rule(healthy_lower)
    },
    hsv = {
      thresholds$hsv_healthy_hue <- c(tc$hsv_healthy_hue_min, tc$hsv_healthy_hue_max)
      thresholds$hsv_min_saturation <- tc$hsv_min_saturation
      hsv_rule()
    },
    exgr = {
      thresholds$exgr_threshold <- tc$exgr_threshold
      exgr_rule()
    },
    vote = {
      votes <- lab_rule(healthy_lower) + hsv_rule() + exgr_rule()
      thresholds$lab_healthy_hue_min <- healthy_lower
      thresholds$exgr_threshold <- tc$exgr_threshold
      votes >= 2
    },
    relative = {
      ref <- leaf_reference_color(lab, achromatic, tc)
      healthy_lower <- ref$hue - tc$relative_hue_shift
      thresholds$reference_lab <- c(L = ref$L, a = ref$a, b = ref$b, hue = ref$hue)
      thresholds$relative_hue_shift <- tc$relative_hue_shift
      thresholds$relative_lightness_drop <- tc$relative_lightness_drop
      details$reference_share <- ref$share
      !achromatic & (ref$hue - lab$hue) <= tc$relative_hue_shift &
        (lab$hue - ref$hue) <= tc$relative_hue_shift * 4 &
        lab$L >= ref$L - tc$relative_lightness_drop
    },
    auto = {
      chrom <- !achromatic
      o <- otsu_threshold(lab$hue[chrom & lab$hue <= tc$lab_healthy_hue_max],
                          config$segmentation$otsu_bins)
      bounds <- tc$auto_hue_bounds
      t <- o$threshold
      clamped <- is.na(t) || t < bounds[1] || t > bounds[2]
      if (is.na(t)) t <- tc$lab_healthy_hue_min
      t <- min(max(t, bounds[1]), bounds[2])
      healthy_lower <- t
      thresholds$lab_healthy_hue_min <- t
      details$otsu_hue_threshold <- o$threshold
      details$hue_separability <- o$separability
      details$threshold_clamped <- clamped
      lab_rule(t)
    }
  )
  thresholds$achromatic_chroma_max <- tc$achromatic_chroma_max
  thresholds$lab_healthy_hue_max <- tc$lab_healthy_hue_max

  cls <- ifelse(healthy, 1L, 2L)
  if (tc$min_injury_size > 0) {
    tmp <- matrix(FALSE, d[1], d[2]); tmp[idx] <- cls == 2L
    kept <- remove_small_regions(tmp, tc$min_injury_size)
    relabel <- tmp[idx] & !kept[idx]
    cls[relabel] <- 1L
    details$relabelled_small_injury_pixels <- sum(relabel)
  }
  if (isTRUE(multiclass)) {
    inj <- cls == 2L
    hue <- lab$hue
    necrotic <- (achromatic & lab$L <= tc$dark_lightness_max) |
      (!achromatic & (hue < tc$lab_necrotic_hue_max | hue >= 300))
    chlorotic <- !achromatic & !necrotic & hue >= tc$lab_necrotic_hue_max &
      hue < healthy_lower
    sub <- ifelse(necrotic, 3L, ifelse(chlorotic, 2L, 4L))
    cls[inj] <- sub[inj]
    thresholds$lab_necrotic_hue_max <- tc$lab_necrotic_hue_max
    thresholds$dark_lightness_max <- tc$dark_lightness_max
    details$multiclass_rule <- "injured = chlorotic + necrotic + other (Lab rules on not-healthy pixels)"
  }
  class_map[idx] <- cls
  structure(list(class_map = class_map, classes = classes,
                 healthy_mask = class_map == 1L,
                 injury_mask = class_map >= 2L,
                 method = method, multiclass = isTRUE(multiclass),
                 thresholds = thresholds, details = details),
            class = "leaf_tissue")
}

#' Dominant ("reference") colour of a leaf
#'
#' Densest cell of the 2-D (a*, b*) histogram of chromatic leaf pixels
#' (bin width `relative_bin_width`); the reference is the median L*a*b* of
#' pixels in that cell. Assumes that the most frequent colour of the leaf is
#' its unaffected tissue, which fails for leaves where injury dominates; the
#' share of pixels supporting the reference is stored for auditing.
#' @noRd
leaf_reference_color <- function(lab, achromatic, tc) {
  sel <- !achromatic
  if (sum(sel) < 10) sel <- rep(TRUE, length(lab$a))
  bw <- tc$relative_bin_width
  key <- paste(floor(lab$a[sel] / bw), floor(lab$b[sel] / bw))
  tab <- table(key)
  top <- names(tab)[which.max(tab)]
  in_top <- which(sel)[key == top]
  # widen to the 3x3 neighbourhood of the densest cell for robustness
  ka <- floor(lab$a / bw); kb <- floor(lab$b / bw)
  ta <- as.numeric(strsplit(top, " ")[[1]])
  near <- sel & abs(ka - ta[1]) <= 1 & abs(kb - ta[2]) <= 1
  a <- stats::median(lab$a[near]); b <- stats::median(lab$b[near])
  hue <- atan2(b, a) * 180 / pi; if (hue < 0) hue <- hue + 360
  list(L = stats::median(lab$L[near]), a = a, b = b, hue = hue, share = mean(near))
}
