#' Paths to the example leaf photographs
#'
#' Two photographs of detached leaves with visible injury on a white
#' background are shipped as **functional examples**. They have no manual
#' reference masks and must **not** be used as ground truth.
#'
#' @return Character vector with the two file paths.
#' @examples
#' leaf_example_images()
#' @export
leaf_example_images <- function() {
  f <- c("leaf_example_01.jpg", "leaf_example_02.jpg")
  p <- system.file("extdata", f, package = "LeafInjuryR")
  if (any(p == "")) leaf_abort("Example images not found; is LeafInjuryR installed correctly?")
  p
}

#' Create a synthetic leaf image with known masks
#'
#' Generates a small synthetic image (white background, green elliptical
#' "leaf", a brown circular "necrotic" spot and optionally a yellow
#' "chlorotic" band) together with the exact masks. It is intended for
#' unit tests and tutorials: because the true masks are known, it allows
#' checking denominators, masks and percentages mathematically. It says
#' nothing about performance on real photographs.
#'
#' @param width,height Image size in pixels.
#' @param necrotic_radius Radius of the brown spot (pixels); 0 for none.
#' @param chlorotic Logical; add a yellow band.
#' @param noise Standard deviation of Gaussian noise added to each channel.
#' @param seed Seed for the noise.
#' @return A list with `image` (colour `Image`), `leaf_mask`, `injury_mask`,
#'   `necrotic_mask`, `chlorotic_mask`, `healthy_mask` and `colors`.
#' @examples
#' syn <- make_synthetic_leaf()
#' sum(syn$injury_mask) / sum(syn$leaf_mask) * 100
#' @export
make_synthetic_leaf <- function(width = 120L, height = 90L, necrotic_radius = 8,
                                chlorotic = FALSE, noise = 0.01, seed = 1L) {
  cols <- list(background = c(0.94, 0.94, 0.93), healthy = c(0.30, 0.60, 0.12),
               necrotic = c(0.42, 0.25, 0.10), chlorotic = c(0.85, 0.72, 0.20))
  x <- matrix(seq_len(width), width, height)
  y <- matrix(seq_len(height), width, height, byrow = TRUE)
  cx <- width / 2; cy <- height / 2
  leaf <- ((x - cx) / (width * 0.35))^2 + ((y - cy) / (height * 0.35))^2 <= 1
  necro <- leaf & ((x - cx * 0.8)^2 + (y - cy)^2 <= necrotic_radius^2)
  chlor <- if (isTRUE(chlorotic)) leaf & !necro & x > cx * 1.25 & x <= cx * 1.4 else leaf & FALSE
  arr <- array(0, c(width, height, 3))
  for (ch in 1:3) {
    layer <- matrix(cols$background[ch], width, height)
    layer[leaf] <- cols$healthy[ch]
    layer[chlor] <- cols$chlorotic[ch]
    layer[necro] <- cols$necrotic[ch]
    arr[, , ch] <- layer
  }
  if (noise > 0) {
    has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    old <- if (has_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
    set.seed(seed)
    arr <- arr + stats::rnorm(length(arr), 0, noise)
    if (has_seed) assign(".Random.seed", old, envir = globalenv())
    else rm(".Random.seed", envir = globalenv())
    arr[arr < 0] <- 0; arr[arr > 1] <- 1
  }
  list(image = as_color_image(arr), leaf_mask = leaf, injury_mask = necro | chlor,
       necrotic_mask = necro, chlorotic_mask = chlor, healthy_mask = leaf & !necro & !chlor,
       colors = cols)
}
