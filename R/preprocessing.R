#' Conservative illumination / colour normalisation
#'
#' Optional pre-processing step that corrects (a) colour casts caused by white
#' balance or artificial lighting and (b) smooth illumination gradients. Both
#' corrections are estimated **only from background pixels**, identified with
#' a preliminary unsupervised segmentation (`method = "auto"` without
#' morphology), so the leaf colours themselves do not drive the correction.
#'
#' * **White balance**: per-channel gains are computed so that the bright
#'   background reference becomes neutral grey while keeping its mean
#'   intensity (no global brightening).
#' * **Illumination flattening** (off by default): a low-order polynomial
#'   surface is fitted to background luminance and each pixel is divided by
#'   the relative surface value.
#'
#' All gains are clipped to `[1 / max_gain, max_gain]` (see [leaf_config()]),
#' which keeps the correction conservative. The input image is never modified:
#' a new image is returned and the estimated corrections are attached as the
#' attribute `"normalization"`. Normalisation cannot recover saturated
#' (clipped) highlights or deep shadows.
#'
#' @param image Image or path.
#' @param white_balance,flatten_illumination Logical; override the
#'   corresponding [leaf_config()] entries when not `NULL`.
#' @param config A [leaf_config()] object.
#' @return A new colour `Image` with attribute `normalization` (list with
#'   `white_balance_gains`, `illumination_surface_range`, `background_pixels`,
#'   `applied`).
#' @examples
#' img <- read_leaf(leaf_example_images()[1])
#' img_n <- normalize_leaf_image(img)
#' attr(img_n, "normalization")$white_balance_gains
#' @export
normalize_leaf_image <- function(image, white_balance = NULL,
                                 flatten_illumination = NULL,
                                 config = leaf_config()) {
  config <- as_leaf_config(config)
  nc <- config$normalization
  wb <- white_balance %||% nc$white_balance
  fl <- flatten_illumination %||% nc$flatten_illumination
  image <- resolve_image(image)
  md <- attr(image, "leaf_metadata")
  arr <- as_rgb_array(image)
  d <- dim(arr)

  pre <- raw_segmentation(arr, "auto", NULL, config)
  bg <- !pre$mask
  info <- list(applied = character(0), white_balance_gains = c(R = 1, G = 1, B = 1),
               illumination_surface_range = c(1, 1), background_pixels = sum(bg))
  if (sum(bg) < 100) {
    info$message <- "Too few background pixels; normalisation not applied."
    out <- as_color_image(arr)
    attr(out, "leaf_metadata") <- md
    attr(out, "normalization") <- info
    return(out)
  }
  max_gain <- nc$max_gain

  if (isTRUE(fl)) {
    lum <- (arr[, , 1] + arr[, , 2] + arr[, , 3]) / 3
    surf <- fit_background_surface(lum, bg, config)
    rel <- surf / stats::median(surf[bg])
    rel <- pmin(pmax(rel, 1 / max_gain), max_gain)
    for (ch in 1:3) arr[, , ch] <- arr[, , ch] / rel
    arr[arr > 1] <- 1
    info$illumination_surface_range <- range(rel)
    info$applied <- c(info$applied, "flatten_illumination")
  }
  if (isTRUE(wb)) {
    lum <- (arr[, , 1] + arr[, , 2] + arr[, , 3]) / 3
    cut <- stats::quantile(lum[bg], nc$background_quantile, names = FALSE)
    ref_sel <- bg & lum >= cut
    ref <- c(stats::median(arr[, , 1][ref_sel]), stats::median(arr[, , 2][ref_sel]),
             stats::median(arr[, , 3][ref_sel]))
    gains <- mean(ref) / pmax(ref, .Machine$double.eps)
    gains <- pmin(pmax(gains, 1 / max_gain), max_gain)
    for (ch in 1:3) arr[, , ch] <- arr[, , ch] * gains[ch]
    arr[arr > 1] <- 1
    info$white_balance_gains <- stats::setNames(gains, c("R", "G", "B"))
    info$applied <- c(info$applied, "white_balance")
  }
  out <- as_color_image(arr)
  attr(out, "leaf_metadata") <- md
  attr(out, "normalization") <- info
  out
}

#' Fit a polynomial surface to a matrix using only selected pixels
#' @noRd
fit_background_surface <- function(values, select, config) {
  sc <- config$segmentation
  d <- dim(values)
  xs <- (row(values) - 1) / max(1, d[1] - 1) * 2 - 1
  ys <- (col(values) - 1) / max(1, d[2] - 1) * 2 - 1
  design <- function(x, y, deg) {
    cols <- list()
    for (i in 0:deg) for (j in 0:(deg - i)) cols[[length(cols) + 1L]] <- x^i * y^j
    do.call(cbind, cols)
  }
  idx <- which(select)
  idx <- idx[sample_indices(length(idx), sc$surface_sample, sc$seed)]
  X <- design(xs[idx], ys[idx], sc$surface_degree)
  fit <- stats::lm.fit(X, values[idx])
  coef <- fit$coefficients
  coef[is.na(coef)] <- 0
  pred <- design(as.vector(xs), as.vector(ys), sc$surface_degree) %*% coef
  matrix(pred, d[1], d[2])
}
