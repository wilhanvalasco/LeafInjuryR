# Internal utilities ---------------------------------------------------------
#
# Nothing in this file is exported. Helpers are kept small and vectorised.

#' Signal a classed LeafInjuryR error
#'
#' @param message Human-readable message.
#' @param class Additional condition class.
#' @noRd
leaf_abort <- function(message, class = NULL) {
  cond <- structure(
    class = c(class, "leafinjury_error", "error", "condition"),
    list(message = message, call = NULL)
  )
  stop(cond)
}

#' Signal a classed LeafInjuryR warning (used sparingly)
#' @noRd
leaf_warn <- function(message) {
  cond <- structure(
    class = c("leafinjury_warning", "warning", "condition"),
    list(message = message, call = NULL)
  )
  warning(cond)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Extract a numeric [width, height, 3] array in [0, 1] from an image
#' @noRd
as_rgb_array <- function(image) {
  if (inherits(image, "Image")) {
    arr <- EBImage::imageData(image)
  } else if (is.array(image)) {
    arr <- image
  } else {
    leaf_abort("`image` must be an EBImage 'Image' or a numeric array [width, height, 3].",
               "leafinjury_invalid_image")
  }
  d <- dim(arr)
  if (length(d) != 3L || d[3] < 3L) {
    leaf_abort("Image has no compatible colour channels (an RGB image is required).",
               "leafinjury_invalid_channels")
  }
  if (d[3] > 3L) arr <- arr[, , 1:3, drop = FALSE]
  storage.mode(arr) <- "double"
  if (anyNA(arr)) arr[is.na(arr)] <- 0
  arr[arr < 0] <- 0
  arr[arr > 1] <- 1
  arr
}

#' Build a colour EBImage Image from an array
#' @noRd
as_color_image <- function(arr) {
  EBImage::Image(arr, colormode = EBImage::Color)
}

#' Validate a logical mask against image dimensions
#' @noRd
check_mask <- function(mask, dims = NULL, name = "mask") {
  if (inherits(mask, "Image")) mask <- EBImage::imageData(mask)
  if (length(dim(mask)) == 3L) mask <- mask[, , 1]
  if (!is.matrix(mask)) {
    leaf_abort(sprintf("`%s` must be a matrix [width, height].", name))
  }
  mask <- mask > 0
  mask[is.na(mask)] <- FALSE
  if (!is.null(dims) && !identical(as.integer(dim(mask)), as.integer(dims[1:2]))) {
    leaf_abort(sprintf("`%s` dimensions (%s) do not match the image (%s).", name,
                       paste(dim(mask), collapse = " x "),
                       paste(dims[1:2], collapse = " x ")))
  }
  mask
}

#' Otsu threshold for an arbitrary numeric vector
#'
#' Returns the threshold that maximises between-class variance and the
#' separability index eta = between-class variance / total variance (0-1).
#' eta is a heuristic indicator of how bimodal the feature is.
#' @noRd
otsu_threshold <- function(x, bins = 256L) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(list(threshold = NA_real_, separability = 0))
  rng <- range(x)
  if (diff(rng) <= .Machine$double.eps) {
    return(list(threshold = rng[1], separability = 0))
  }
  breaks <- seq(rng[1], rng[2], length.out = bins + 1L)
  mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  idx <- findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE)
  counts <- tabulate(idx, nbins = bins)
  p <- counts / sum(counts)
  omega <- cumsum(p)
  mu <- cumsum(p * mids)
  mu_t <- mu[bins]
  denom <- omega * (1 - omega)
  sigma_b <- ifelse(denom > 0, (mu_t * omega - mu)^2 / denom, 0)
  k <- which.max(sigma_b)
  total_var <- sum(p * (mids - mu_t)^2)
  eta <- if (total_var > 0) sigma_b[k] / total_var else 0
  list(threshold = breaks[k + 1L], separability = as.numeric(eta))
}

#' Pixels on the image border (fraction of each side)
#' @noRd
border_mask <- function(dims, fraction = 0.05) {
  w <- dims[1]; h <- dims[2]
  bw <- max(1L, as.integer(round(w * fraction)))
  bh <- max(1L, as.integer(round(h * fraction)))
  m <- matrix(FALSE, w, h)
  m[seq_len(bw), ] <- TRUE
  m[(w - bw + 1L):w, ] <- TRUE
  m[, seq_len(bh)] <- TRUE
  m[, (h - bh + 1L):h] <- TRUE
  m
}

#' Deterministic subsample of indices
#' @noRd
sample_indices <- function(n, size, seed) {
  if (n <= size) return(seq_len(n))
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv()), add = TRUE)
  }
  set.seed(seed)
  sort(sample.int(n, size))
}

#' Make a morphological brush from a radius (0 = no operation)
#' @noRd
make_brush <- function(radius, shape = "disc") {
  size <- 2L * as.integer(radius) + 1L
  EBImage::makeBrush(size, shape = shape)
}

#' Safe percentage
#' @noRd
safe_percent <- function(num, den) {
  if (is.na(den) || den <= 0) return(NA_real_)
  100 * num / den
}

#' Installed package version as character
#' @noRd
pkg_version <- function(pkg) {
  v <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
  v
}

#' Evaluate an expression with a fixed seed, restoring the RNG state
#' @noRd
with_seed <- function(seed, expr) {
  has <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (has) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit(if (has) assign(".Random.seed", old, envir = globalenv())
          else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
            rm(".Random.seed", envir = globalenv()), add = TRUE)
  set.seed(seed)
  expr
}
