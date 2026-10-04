#' Default class colours
#'
#' @param multiclass Logical.
#' @return Named character vector of hex colours for non-background classes.
#' @noRd
leaf_class_colors <- function(multiclass = FALSE) {
  if (isTRUE(multiclass)) {
    c(healthy = "#1FA83A", chlorotic = "#FFD500", necrotic = "#E31A1C", other = "#C21BFF")
  } else {
    c(healthy = "#1FA83A", injured = "#E31A1C")
  }
}

#' Create a translucent classification overlay
#'
#' Blends class colours over the image (healthy = translucent green, injured
#' = translucent red; multiclass: healthy green, chlorotic yellow, necrotic
#' red, other magenta). Background pixels are left untouched so the
#' photograph remains visible. The input image is not modified; a new image is
#' returned.
#'
#' @param image Image, path or `leaf_analysis` object.
#' @param class_map Integer class map; taken from `image` when it is a
#'   `leaf_analysis`.
#' @param alpha Opacity of the colour layer (0-1).
#' @param colors Optional named colours (see [leaf_class_colors()]).
#' @param multiclass Logical; inferred when `NULL`.
#' @param outline Draw the leaf-mask outline (logical).
#' @return A colour `Image`.
#' @noRd
create_leaf_overlay <- function(image, class_map = NULL, alpha = 0.45, colors = NULL,
                                multiclass = NULL, outline = TRUE) {
  if (inherits(image, "leaf_analysis")) {
    class_map <- class_map %||% image$class_map
    multiclass <- multiclass %||% image$parameters$multiclass
    image <- image$cropped
  }
  if (is.null(class_map)) leaf_abort("`class_map` is required.")
  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) leaf_abort("`alpha` must be in [0, 1].")
  arr <- as_rgb_array(resolve_image(image))
  d <- dim(arr)
  if (!identical(as.integer(dim(class_map)), as.integer(d[1:2]))) {
    leaf_abort("`class_map` dimensions do not match the image.")
  }
  multiclass <- multiclass %||% any(class_map > 2L)
  colors <- colors %||% leaf_class_colors(multiclass)
  rgbm <- grDevices::col2rgb(colors) / 255
  out <- arr
  for (k in seq_along(colors)) {
    sel <- which(class_map == k)
    if (!length(sel)) next
    for (ch in 1:3) {
      layer <- out[, , ch]
      layer[sel] <- (1 - alpha) * layer[sel] + alpha * rgbm[ch, k]
      out[, , ch] <- layer
    }
  }
  if (isTRUE(outline)) {
    leaf <- class_map > 0
    if (any(leaf)) {
      edge <- leaf & !(EBImage::erode(leaf * 1, EBImage::makeBrush(3, "diamond")) > 0)
      for (ch in 1:3) { layer <- out[, , ch]; layer[edge] <- c(0.1, 0.1, 0.1)[ch]; out[, , ch] <- layer }
    }
  }
  as_color_image(out)
}

#' Render a class map as a solid-colour image
#'
#' @param class_map Integer class map.
#' @param multiclass Logical; inferred when `NULL`.
#' @param background Background colour.
#' @return A colour `Image`.
#' @noRd
class_map_to_image <- function(class_map, multiclass = NULL, background = "#FFFFFF") {
  multiclass <- multiclass %||% any(class_map > 2L)
  cols <- grDevices::col2rgb(c(background, leaf_class_colors(multiclass))) / 255
  d <- dim(class_map)
  out <- array(0, c(d, 3))
  for (ch in 1:3) out[, , ch] <- cols[ch, class_map + 1L]
  as_color_image(out)
}

#' Render a logical mask as a black/white image
#' @param mask Logical matrix.
#' @return A grayscale `Image` (leaf = white).
#' @noRd
mask_to_image <- function(mask) {
  EBImage::Image(check_mask(mask) * 1, colormode = EBImage::Grayscale)
}

#' Plot an image with pixel coordinates
#'
#' Draws an image with base graphics. The plotting coordinates equal pixel
#' coordinates (x to the right, y downwards), which the Shiny interface uses
#' for manual cropping.
#'
#' @param image Image, array or logical matrix.
#' @param main Title.
#' @param axes Draw axes.
#' @param ... Passed to [graphics::title()].
#' @return `NULL`, invisibly.
#' @noRd
plot_leaf_image <- function(image, main = NULL, axes = FALSE, ...) {
  if (is.matrix(image) || (inherits(image, "Image") && length(dim(image)) == 2L)) {
    m <- if (inherits(image, "Image")) EBImage::imageData(image) else image * 1
    arr <- array(m, c(dim(m), 3))
  } else {
    arr <- as_rgb_array(image)
  }
  d <- dim(arr)
  ras <- grDevices::as.raster(aperm(arr, c(2, 1, 3)))
  op <- graphics::par(mar = c(if (axes) 2 else 0.2, if (axes) 2 else 0.2,
                              if (is.null(main)) 0.2 else 1.6, 0.2))
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0.5, d[1] + 0.5), ylim = c(d[2] + 0.5, 0.5),
                        asp = 1, xaxs = "i", yaxs = "i")
  graphics::rasterImage(ras, 0.5, d[2] + 0.5, d[1] + 0.5, 0.5, interpolate = FALSE)
  if (axes) { graphics::axis(1); graphics::axis(2) }
  if (!is.null(main)) graphics::title(main = main, line = 0.4, ...)
  invisible(NULL)
}

#' Plot a leaf analysis (four audit panels)
#'
#' Shows ORIGINAL (analysed region), LEAF MASK, CLASSIFICATION and OVERLAY.
#'
#' @param x A `leaf_analysis` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' res <- analyze_leaf(system.file("extdata", "leaf_example_01.jpg", package = "LeafInjuryR"), crop = "auto")
#' plot(res)
#' @export
plot.leaf_analysis <- function(x, ...) {
  op <- graphics::par(mfrow = c(2, 2))
  on.exit(graphics::par(op))
  plot_leaf_image(x$cropped, main = "Original")
  plot_leaf_image(x$leaf_mask, main = "Leaf mask")
  plot_leaf_image(class_map_to_image(x$class_map, x$parameters$multiclass),
                  main = "Classification")
  plot_leaf_image(x$overlay, main = sprintf("Overlay - injured %.2f%%",
                                            x$metrics$injured_percent))
  invisible(x)
}
