# Internal helpers for the interactive viewer (Shiny) -------------------------

#' Pre-computed display raster (down-scaled for drawing speed)
#'
#' The raster is drawn stretched over the full-resolution pixel extent, so
#' plot coordinates always equal original pixel coordinates (needed for
#' manual cropping and corrections), whatever the display size.
#' @noRd
display_raster <- function(x, max_dim = 1600L) {
  if (is.null(x)) return(NULL)
  if (is.matrix(x) && is.logical(x)) {
    arr <- array(x * 1, c(dim(x), 3))
  } else {
    arr <- as_rgb_array(x)
  }
  d <- dim(arr)
  s <- min(1, max_dim / max(d[1:2]))
  small <- if (s < 1) EBImage::imageData(EBImage::resize(as_color_image(arr),
                                                         w = max(1, round(d[1] * s)),
                                                         h = max(1, round(d[2] * s)))) else arr
  list(raster = grDevices::as.raster(aperm(small, c(2, 1, 3))), dims = d[1:2])
}

#' Draw a display raster with optional zoom window, grid and rectangle
#' @noRd
draw_leaf_view <- function(view, xlim = NULL, ylim = NULL, grid = FALSE, rect = NULL,
                           rect_col = "#3E8E5E", bg = "#FFFFFF") {
  d <- view$dims
  xlim <- xlim %||% c(0.5, d[1] + 0.5)
  ylim <- ylim %||% c(0.5, d[2] + 0.5)
  op <- graphics::par(mar = c(0, 0, 0, 0), bg = bg, xaxs = "i", yaxs = "i")
  on.exit(graphics::par(op))
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = rev(ylim), asp = 1)
  graphics::rasterImage(view$raster, 0.5, d[2] + 0.5, d[1] + 0.5, 0.5, interpolate = TRUE)
  if (isTRUE(grid)) {
    step <- diff(pretty(c(0, max(d)), 10))[1]
    xs <- seq(step, d[1], by = step); ys <- seq(step, d[2], by = step)
    graphics::segments(xs, 0.5, xs, d[2] + 0.5, col = grDevices::adjustcolor("#FFFFFF", 0.55), lwd = 0.6)
    graphics::segments(0.5, ys, d[1] + 0.5, ys, col = grDevices::adjustcolor("#FFFFFF", 0.55), lwd = 0.6)
    graphics::segments(xs, 0.5, xs, d[2] + 0.5, col = grDevices::adjustcolor("#000000", 0.18), lwd = 0.4, lty = 3)
    graphics::segments(0.5, ys, d[1] + 0.5, ys, col = grDevices::adjustcolor("#000000", 0.18), lwd = 0.4, lty = 3)
  }
  if (!is.null(rect)) {
    graphics::rect(rect[["x_min"]], rect[["y_max"]], rect[["x_max"]], rect[["y_min"]],
                   border = rect_col, lwd = 2, lty = 2)
  }
  invisible(NULL)
}

#' Image for a given view of a segmentation / analysis
#' @noRd
view_image <- function(base, seg, view = c("overlay", "classes", "mask", "original"), alpha = 0.45) {
  view <- match.arg(view)
  if (is.null(seg) || view == "original") return(base)
  switch(view,
    overlay = create_leaf_overlay(base, seg$class_map, alpha = alpha, multiclass = seg$multiclass),
    classes = class_map_to_image(seg$class_map, seg$multiclass, background = "#F2F2F0"),
    mask = seg$leaf_mask)
}

#' Clamp a zoom window to the image
#' @noRd
clamp_window <- function(cx, cy, half_w, half_h, dims) {
  half_w <- min(half_w, dims[1] / 2); half_h <- min(half_h, dims[2] / 2)
  cx <- min(max(cx, 0.5 + half_w), dims[1] + 0.5 - half_w)
  cy <- min(max(cy, 0.5 + half_h), dims[2] + 0.5 - half_h)
  list(xlim = c(cx - half_w, cx + half_w), ylim = c(cy - half_h, cy + half_h))
}
