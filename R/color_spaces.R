#' Convert sRGB values to CIELAB
#'
#' Vectorised conversion from sRGB (values in \[0, 1\], D65 white point) to
#' CIE L*a*b* via CIE XYZ. L* is lightness (0-100), a* the green (-) to red
#' (+) axis and b* the blue (-) to yellow (+) axis. Chroma C* = sqrt(a*^2 +
#' b*^2) and hue angle h = atan2(b*, a*) in degrees (0-360) are also returned.
#'
#' Camera images are rarely colorimetrically calibrated, so L*a*b* values
#' computed here are *device-dependent approximations*, adequate for relative
#' comparisons within a standardised imaging setup.
#'
#' @param r,g,b Numeric vectors or matrices in \[0, 1\].
#' @return A list with `L`, `a`, `b`, `chroma`, `hue` (same shape as input).
#' @noRd
rgb_to_lab <- function(r, g, b) {
  lin <- function(u) {
    out <- u / 12.92
    hi <- u > 0.04045
    out[hi] <- ((u[hi] + 0.055) / 1.055)^2.4
    out
  }
  rl <- lin(r); gl <- lin(g); bl <- lin(b)
  x <- (0.4124564 * rl + 0.3575761 * gl + 0.1804375 * bl) / 0.95047
  y <- (0.2126729 * rl + 0.7151522 * gl + 0.0721750 * bl)
  z <- (0.0193339 * rl + 0.1191920 * gl + 0.9503041 * bl) / 1.08883
  f <- function(t) {
    out <- 7.787 * t + 16 / 116
    hi <- t > 0.008856
    out[hi] <- t[hi]^(1 / 3)
    out
  }
  fx <- f(x); fy <- f(y); fz <- f(z)
  L <- 116 * fy - 16
  a <- 500 * (fx - fy)
  bb <- 200 * (fy - fz)
  hue <- atan2(bb, a) * 180 / pi
  hue[hue < 0] <- hue[hue < 0] + 360
  list(L = L, a = a, b = bb, chroma = sqrt(a^2 + bb^2), hue = hue)
}

#' Convert sRGB values to HSV
#'
#' @param r,g,b Numeric vectors or matrices in \[0, 1\].
#' @return A list with `hue` (degrees, 0-360), `saturation` (0-1) and
#'   `value` (0-1).
#' @noRd
rgb_to_hsv <- function(r, g, b) {
  mx <- pmax(r, g, b)
  mn <- pmin(r, g, b)
  delta <- mx - mn
  hue <- r * 0
  nz <- delta > 0
  is_r <- nz & mx == r
  is_g <- nz & mx == g & !is_r
  is_b <- nz & !is_r & !is_g
  hue[is_r] <- ((g[is_r] - b[is_r]) / delta[is_r]) %% 6
  hue[is_g] <- (b[is_g] - r[is_g]) / delta[is_g] + 2
  hue[is_b] <- (r[is_b] - g[is_b]) / delta[is_b] + 4
  hue <- hue * 60
  sat <- ifelse(mx > 0, delta / mx, 0)
  list(hue = hue, saturation = sat, value = mx)
}

#' Vegetation colour indices
#'
#' Computes excess green (ExG = 2g - r - b), excess red (ExR = 1.4r - g) and
#' ExG - ExR on chromatic coordinates (r = R/(R+G+B), etc.).
#'
#' @param r,g,b Numeric vectors or matrices in \[0, 1\].
#' @return A list with `exg`, `exr`, `exgr`.
#' @noRd
vegetation_indices <- function(r, g, b) {
  s <- r + g + b
  s[s <= 0] <- 1
  rn <- r / s; gn <- g / s; bn <- b / s
  exg <- 2 * gn - rn - bn
  exr <- 1.4 * rn - gn
  list(exg = exg, exr = exr, exgr = exg - exr)
}

#' Compute all colour features of an image
#' @noRd
compute_color_features <- function(arr) {
  r <- arr[, , 1]; g <- arr[, , 2]; b <- arr[, , 3]
  lab <- rgb_to_lab(r, g, b)
  hsv <- rgb_to_hsv(r, g, b)
  vi <- vegetation_indices(r, g, b)
  list(L = lab$L, a = lab$a, b = lab$b, chroma = lab$chroma, lab_hue = lab$hue,
       hsv_hue = hsv$hue, saturation = hsv$saturation, value = hsv$value,
       exg = vi$exg, exr = vi$exr, exgr = vi$exgr,
       gray = (r + g + b) / 3)
}

#' Colour features of pixels inside a mask
#'
#' Returns a data frame with one row per pixel inside `mask` (optionally
#' subsampled) and columns for CIELAB (L, a, b, chroma, hue), HSV (hue,
#' saturation, value) and vegetation indices (ExG, ExR, ExG-ExR). Useful for
#' exploring which features separate healthy from injured tissue in a given
#' imaging setup; no single channel is assumed to be universally superior.
#'
#' @param image Image or path.
#' @param mask Optional logical matrix; defaults to all pixels.
#' @param max_pixels Maximum number of rows returned (random subsample with
#'   a fixed seed).
#' @param seed Seed for subsampling.
#' @return A data frame with `x`, `y` and the colour features.
#' @noRd
leaf_color_features <- function(image, mask = NULL, max_pixels = 50000L, seed = 1L) {
  arr <- as_rgb_array(resolve_image(image))
  d <- dim(arr)
  mask <- if (is.null(mask)) matrix(TRUE, d[1], d[2]) else check_mask(mask, d)
  idx <- which(mask)
  idx <- idx[sample_indices(length(idx), max_pixels, seed)]
  feats <- compute_color_features(arr)
  out <- data.frame(x = (idx - 1L) %% d[1] + 1L, y = (idx - 1L) %/% d[1] + 1L)
  for (nm in names(feats)) out[[nm]] <- feats[[nm]][idx]
  out
}
