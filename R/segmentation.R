#' Segment the leaf from the background
#'
#' Produces the **leaf mask**: the set of pixels that belong to the leaf,
#' regardless of whether the tissue is healthy or injured. This step is
#' separate from tissue classification ([classify_leaf_tissue()]); a good
#' segmentation must include necrotic and chlorotic tissue.
#'
#' Thresholds are derived from the image itself (Otsu) unless `threshold` is
#' given. Which side of a threshold is background is inferred from the image
#' border (`border_fraction` in [leaf_config()]): the side containing most
#' border pixels is background. This works for light or dark backgrounds.
#'
#' @section Methods:
#' \describe{
#'   \item{`"lab"`}{Otsu threshold on CIELAB chroma C*. Good for achromatic
#'     (white/grey/black) backgrounds; may miss dark, low-chroma necrosis.}
#'   \item{`"hsv"`}{Otsu threshold on HSV saturation.}
#'   \item{`"exg"`}{Otsu threshold on excess green (ExG); foreground = high
#'     ExG. Tends to exclude brown necrotic tissue (shown for comparison).}
#'   \item{`"otsu"`}{Otsu threshold on grey intensity.}
#'   \item{`"adaptive"`}{Lightness after removing a smooth illumination
#'     surface fitted to preliminary background pixels, then Otsu. Useful with
#'     illumination gradients.}
#'   \item{`"kmeans"`}{k-means (default 3 clusters) on standardised L*a*b*
#'     (sampled, fixed seed); clusters holding a substantial share of the
#'     border pixels are background, the others foreground.}
#'   \item{`"auto"`}{Evaluates chroma C* and darkness (100 - L*). Each
#'     feature is accepted when its Otsu separability is at least
#'     `min_separability` and it leaves at most `max_border_foreground` of the
#'     border as foreground. The leaf mask is the union of accepted features
#'     (chroma captures green/yellow tissue; darkness captures dark necrosis).
#'     If no feature is accepted, k-means is used. The decision is recorded in
#'     `details`.}
#' }
#' After the raw mask, [clean_leaf_mask()] applies morphology, connected
#' components and hole handling.
#'
#' @param image Image or path.
#' @param method Segmentation method, see *Methods*.
#' @param threshold Optional manual threshold on the method's primary feature
#'   (chroma for `"lab"`, saturation for `"hsv"`, ExG for `"exg"`, grey level
#'   for `"otsu"`, corrected lightness residual for `"adaptive"`). Not
#'   applicable to `"auto"` and `"kmeans"`.
#' @param clean Apply [clean_leaf_mask()] (default `TRUE`).
#' @param config A [leaf_config()] object.
#' @return An object of class `leaf_segmentation`: a list with `mask` (final
#'   logical matrix), `raw_mask`, `method`, `thresholds`, `separability`,
#'   `details`, `components`, `holes`, `background_lab`, `n_raw_components`,
#'   `main_component_share`.
#' @noRd
segment_leaf_mask <- function(image, method = "auto", threshold = NULL, clean = TRUE,
                         config = leaf_config()) {
  config <- as_leaf_config(config)
  method <- match.arg(method, leaf_methods()$method)
  arr <- as_rgb_array(resolve_image(image))
  threshold <- threshold %||% config$segmentation$threshold
  raw <- raw_segmentation(arr, method, threshold, config)
  bg_lab <- estimate_background_lab(arr, !raw$mask, config)
  if (isTRUE(clean)) {
    cl <- clean_leaf_mask(raw$mask, arr, bg_lab, config)
  } else {
    d <- dim(raw$mask)
    cl <- list(mask = raw$mask, components = data.frame(), n_raw_components = NA_integer_,
               main_component_share = NA_real_, min_object_size_used = NA_real_,
               holes = list(policy = "none", hole_pixels = 0L, filled_pixels = 0L,
                            perforation_candidate_pixels = 0L,
                            perforation_mask = matrix(FALSE, d[1], d[2])))
  }
  structure(list(
    mask = cl$mask, raw_mask = raw$mask, method = method,
    thresholds = raw$thresholds, separability = raw$separability,
    details = raw$details, components = cl$components, holes = cl$holes,
    background_lab = bg_lab, n_raw_components = cl$n_raw_components,
    main_component_share = cl$main_component_share,
    min_object_size_used = cl$min_object_size_used
  ), class = "leaf_segmentation")
}

#' Threshold a feature with background polarity inferred from the border
#' @noRd
polarity_split <- function(feature, t, border, fixed_high = NULL) {
  high <- feature > t
  fg_high <- if (!is.null(fixed_high)) fixed_high else mean(high[border]) < 0.5
  fg <- if (fg_high) high else !high
  list(mask = fg, foreground_is_high = fg_high, border_foreground = mean(fg[border]))
}

#' Raw (uncleaned) segmentation
#' @noRd
raw_segmentation <- function(arr, method, threshold, config) {
  sc <- config$segmentation
  d <- dim(arr)
  border <- border_mask(d, sc$border_fraction)
  if (!is.null(threshold) && method %in% c("auto", "kmeans")) {
    leaf_abort(sprintf("`threshold` is not applicable to method '%s'.", method))
  }
  if (!is.null(threshold) && (!is.numeric(threshold) || length(threshold) != 1L)) {
    leaf_abort("`threshold` must be a single number or NULL.")
  }
  single <- function(feature, name, fixed_high = NULL) {
    o <- otsu_threshold(feature, sc$otsu_bins)
    t <- threshold %||% o$threshold
    ps <- polarity_split(feature, t, border, fixed_high)
    list(mask = ps$mask,
         thresholds = stats::setNames(list(t), name),
         separability = stats::setNames(list(o$separability), name),
         details = list(strategy = method,
                        threshold_source = if (is.null(threshold)) "otsu" else "user",
                        foreground_is_high = ps$foreground_is_high,
                        border_foreground = ps$border_foreground))
  }
  r <- arr[, , 1]; g <- arr[, , 2]; b <- arr[, , 3]
  switch(method,
    lab = single(rgb_to_lab(r, g, b)$chroma, "chroma"),
    hsv = single(rgb_to_hsv(r, g, b)$saturation, "saturation"),
    exg = single(vegetation_indices(r, g, b)$exg, "exg", fixed_high = TRUE),
    otsu = single((r + g + b) / 3, "gray"),
    adaptive = {
      L <- rgb_to_lab(r, g, b)$L
      o0 <- otsu_threshold(L, sc$otsu_bins)
      pre <- polarity_split(L, o0$threshold, border)
      surf <- fit_background_surface(L, !pre$mask, config)
      res <- single(L - surf, "lightness_residual")
      res$details$initial_lightness_threshold <- o0$threshold
      res
    },
    kmeans = kmeans_segmentation(arr, border, config),
    auto = auto_segmentation(arr, border, config)
  )
}

#' @noRd
auto_segmentation <- function(arr, border, config) {
  sc <- config$segmentation
  lab <- rgb_to_lab(arr[, , 1], arr[, , 2], arr[, , 3])
  feats <- list(chroma = lab$chroma, darkness = 100 - lab$L)
  thr <- list(); sep <- list(); acc <- character(0); masks <- list(); pol <- list()
  bfg <- list()
  for (nm in names(feats)) {
    o <- otsu_threshold(feats[[nm]], sc$otsu_bins)
    ps <- polarity_split(feats[[nm]], o$threshold, border)
    thr[[nm]] <- o$threshold; sep[[nm]] <- o$separability
    pol[[nm]] <- ps$foreground_is_high; bfg[[nm]] <- ps$border_foreground
    masks[[nm]] <- ps$mask
    if (o$separability >= sc$min_separability &&
        ps$border_foreground <= sc$max_border_foreground) acc <- c(acc, nm)
  }
  if (length(acc) == 0L) {
    km <- kmeans_segmentation(arr, border, config)
    km$details$strategy <- "auto:kmeans_fallback"
    km$details$rejected_features <- names(feats)
    km$separability <- c(km$separability, sep)
    return(km)
  }
  mask <- Reduce(`|`, masks[acc])
  list(mask = mask, thresholds = thr[acc], separability = sep,
       details = list(strategy = paste0("auto:", paste(acc, collapse = "+")),
                      accepted_features = acc,
                      rejected_features = setdiff(names(feats), acc),
                      foreground_is_high = unlist(pol),
                      border_foreground = unlist(bfg),
                      candidate_thresholds = unlist(thr)))
}

#' @noRd
kmeans_segmentation <- function(arr, border, config) {
  sc <- config$segmentation
  d <- dim(arr)
  lab <- rgb_to_lab(arr[, , 1], arr[, , 2], arr[, , 3])
  X <- cbind(as.vector(lab$L), as.vector(lab$a), as.vector(lab$b))
  mu <- colMeans(X); s <- apply(X, 2, stats::sd); s[s <= 0] <- 1
  Xs <- sweep(sweep(X, 2, mu), 2, s, "/")
  idx <- sample_indices(nrow(Xs), sc$kmeans_sample, sc$seed)
  sub <- Xs[idx, , drop = FALSE]
  k <- min(sc$kmeans_centers, nrow(unique(round(sub, 6))))
  if (k < 2) {
    return(list(mask = matrix(FALSE, d[1], d[2]), thresholds = list(),
                separability = list(kmeans = 0),
                details = list(strategy = "kmeans", note = "image has no colour variation")))
  }
  km <- with_seed(sc$seed, stats::kmeans(sub, centers = k, iter.max = sc$kmeans_iter,
                                         nstart = sc$kmeans_nstart))
  cc <- km$centers
  dist <- vapply(seq_len(k), function(j) rowSums(sweep(Xs, 2, cc[j, ])^2), numeric(nrow(Xs)))
  cl <- matrix(max.col(-dist, ties.method = "first"), d[1], d[2])
  border_share <- tabulate(cl[border], nbins = k) / sum(border)
  bg_clusters <- which(border_share >= sc$kmeans_background_border_share)
  if (!length(bg_clusters)) bg_clusters <- which.max(border_share)
  fg <- !(cl %in% bg_clusters)
  dim(fg) <- d[1:2]
  list(mask = fg, thresholds = list(),
       separability = list(kmeans = km$betweenss / km$totss),
       details = list(strategy = "kmeans", seed = sc$seed, k = k,
                      centers_lab = sweep(sweep(cc, 2, s, "*"), 2, mu, "+"),
                      background_clusters = bg_clusters,
                      cluster_border_share = border_share,
                      border_foreground = mean(fg[border])))
}
