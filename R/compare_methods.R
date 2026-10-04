#' Compare leaf segmentation methods on one image
#'
#' Runs several segmentation methods with the same configuration and returns
#' their masks, areas, heuristic indicators and pairwise agreement (IoU
#' between methods). Agreement between methods is **not** accuracy: without a
#' manual reference mask no method is declared "best". Use
#' [evaluate_segmentation()] with ground truth for that.
#'
#' @param image Image or path.
#' @param methods Segmentation methods (see [segment_leaf_mask()]).
#' @param config A [leaf_config()] object.
#' @return An object of class `leaf_method_comparison` with `image`,
#'   `masks` (named list), `summary` (data frame), `agreement` (IoU matrix)
#'   and `note`. Has a `plot()` method.
#' @noRd
compare_segmentation_methods <- function(image, methods = c("hsv", "lab", "exg", "otsu", "auto"),
                                         config = leaf_config()) {
  config <- as_leaf_config(config)
  methods <- match.arg(methods, leaf_methods()$method, several.ok = TRUE)
  img <- resolve_image(image)
  arr <- as_rgb_array(img)
  segs <- lapply(methods, function(m) segment_leaf_mask(arr, method = m, config = config))
  names(segs) <- methods
  n <- length(arr[, , 1])
  summary <- do.call(rbind, lapply(methods, function(m) {
    s <- segs[[m]]
    data.frame(method = m, strategy = s$details$strategy %||% m,
               leaf_pixels = sum(s$mask), leaf_fraction = sum(s$mask) / n,
               n_raw_components = s$n_raw_components,
               main_component_share = s$main_component_share,
               separability = if (length(s$separability)) max(unlist(s$separability)) else NA_real_,
               perforation_candidate_pixels = s$holes$perforation_candidate_pixels,
               stringsAsFactors = FALSE)
  }))
  k <- length(methods)
  agree <- matrix(NA_real_, k, k, dimnames = list(methods, methods))
  for (i in seq_len(k)) for (j in seq_len(k)) {
    a <- segs[[i]]$mask; b <- segs[[j]]$mask
    u <- sum(a | b)
    agree[i, j] <- if (u > 0) sum(a & b) / u else NA_real_
  }
  structure(list(image = img, masks = lapply(segs, `[[`, "mask"), summary = summary,
                 agreement = agree,
                 note = "Agreement between methods is not accuracy; no method is declared best without ground truth."),
            class = "leaf_method_comparison")
}

#' @export
print.leaf_method_comparison <- function(x, ...) {
  cat("<leaf_method_comparison>\n")
  print(x$summary, row.names = FALSE)
  cat("\nPairwise IoU between methods (agreement, not accuracy):\n")
  print(round(x$agreement, 3))
  cat("\n", x$note, "\n", sep = "")
  invisible(x)
}

#' @export
plot.leaf_method_comparison <- function(x, alpha = 0.5, ...) {
  k <- length(x$masks) + 1L
  nc <- min(3L, k); nr <- ceiling(k / nc)
  op <- graphics::par(mfrow = c(nr, nc))
  on.exit(graphics::par(op))
  plot_leaf_image(x$image, main = "Original")
  for (m in names(x$masks)) {
    cm <- x$masks[[m]] * 1L
    storage.mode(cm) <- "integer"
    plot_leaf_image(create_leaf_overlay(x$image, cm, alpha = alpha,
                                        colors = c(leaf = "#1F78B4")),
                    main = sprintf("%s (%.1f%% of image)", toupper(m),
                                   100 * mean(x$masks[[m]])))
  }
  invisible(x)
}

#' Compare tissue classification methods inside a fixed leaf mask
#'
#' Keeps the leaf mask fixed so that only the tissue classification differs.
#' As with segmentation, differences between methods do not indicate which
#' one is correct.
#'
#' @param image Image or path.
#' @param leaf_mask Logical leaf mask; if `NULL` it is computed with
#'   `segment_leaf_mask(image, "auto")`.
#' @param methods Tissue methods (see [classify_leaf_tissue()]).
#' @param multiclass Logical.
#' @param config A [leaf_config()] object.
#' @return A list with `class_maps` and `summary` (data frame of percentages).
#' @noRd
compare_tissue_methods <- function(image, leaf_mask = NULL,
                                   methods = c("lab", "hsv", "exgr", "vote", "auto"),
                                   multiclass = FALSE, config = leaf_config()) {
  config <- as_leaf_config(config)
  methods <- match.arg(methods, leaf_methods()$tissue_method, several.ok = TRUE)
  arr <- as_rgb_array(resolve_image(image))
  leaf_mask <- leaf_mask %||% segment_leaf_mask(arr, "auto", config = config)$mask
  res <- lapply(methods, function(m) classify_leaf_tissue(arr, leaf_mask, m, multiclass, config))
  names(res) <- methods
  summary <- do.call(rbind, lapply(methods, function(m) {
    cbind(data.frame(tissue_method = m, stringsAsFactors = FALSE),
          calculate_injury(res[[m]]$class_map, leaf_mask, multiclass))
  }))
  list(class_maps = lapply(res, `[[`, "class_map"), summary = summary,
       note = "Differences between methods do not indicate which one is correct.")
}
