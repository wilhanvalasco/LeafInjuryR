#' Automatically crop the image around the leaf
#'
#' Locates the main leaf on a down-scaled preview (maximum dimension
#' `preview_max_dim`, see [leaf_config()]) using [segment_leaf()], takes the
#' bounding box of the retained component(s), adds a margin and crops the
#' full-resolution image. The bounding box is stored so that masks can be
#' mapped back to the original image with [uncrop_mask()].
#'
#' If no leaf is found the full image is returned with `found = FALSE` in the
#' crop information (no silent failure).
#'
#' @param image Image or path.
#' @param method Segmentation method used to locate the leaf.
#' @param margin Margin as fraction of the bounding-box size; defaults to
#'   `config$crop$margin_fraction`.
#' @param config A [leaf_config()] object.
#' @return The cropped `Image` with attribute `"crop_info"`: a list with
#'   `method`, `found`, `bbox` (`x_min`, `x_max`, `y_min`, `y_max` in original
#'   pixel coordinates) and `original_dim`.
#' @examples
#' img <- read_leaf(leaf_example_images()[1])
#' cr <- auto_crop_leaf(img)
#' attr(cr, "crop_info")$bbox
#' @export
auto_crop_leaf <- function(image, method = "auto", margin = NULL,
                           config = leaf_config()) {
  config <- as_leaf_config(config)
  image <- resolve_image(image)
  md <- attr(image, "leaf_metadata")
  d <- dim(image)
  margin <- margin %||% config$crop$margin_fraction
  scale <- min(1, config$crop$preview_max_dim / max(d[1:2]))
  preview <- if (scale < 1) {
    EBImage::resize(image, w = max(2, round(d[1] * scale)), h = max(2, round(d[2] * scale)))
  } else image
  seg <- segment_leaf(preview, method = method, config = config)
  if (!any(seg$mask)) {
    out <- image
    attr(out, "crop_info") <- list(method = "auto", found = FALSE,
                                   bbox = c(x_min = 1, x_max = d[1], y_min = 1, y_max = d[2]),
                                   original_dim = d[1:2])
    return(out)
  }
  xs <- which(rowSums(seg$mask) > 0)
  ys <- which(colSums(seg$mask) > 0)
  pd <- dim(seg$mask)
  fx <- d[1] / pd[1]; fy <- d[2] / pd[2]
  x_rng <- c((min(xs) - 1) * fx + 1, max(xs) * fx)
  y_rng <- c((min(ys) - 1) * fy + 1, max(ys) * fy)
  mx <- diff(x_rng) * margin; my <- diff(y_rng) * margin
  bbox <- c(x_min = max(1, floor(x_rng[1] - mx)), x_max = min(d[1], ceiling(x_rng[2] + mx)),
            y_min = max(1, floor(y_rng[1] - my)), y_max = min(d[2], ceiling(y_rng[2] + my)))
  out <- crop_by_bbox(image, bbox)
  attr(out, "leaf_metadata") <- md
  attr(out, "crop_info") <- list(method = "auto", found = TRUE, bbox = bbox,
                                 original_dim = d[1:2], margin = margin,
                                 locate_method = method)
  out
}

#' Manually crop an image
#'
#' Crops to the bounding box of the supplied points (e.g. corners clicked or a
#' rectangle drawn in the Shiny interface). Coordinates are in pixels, with
#' `x` along the width and `y` along the height, origin at the top-left.
#' Values outside the image are clipped to its borders.
#'
#' @param image Image or path.
#' @param x,y Numeric vectors (at least two points each) of pixel coordinates.
#' @return The cropped `Image` with attribute `"crop_info"` (see
#'   [auto_crop_leaf()]).
#' @examples
#' img <- read_leaf(leaf_example_images()[1])
#' cr <- crop_leaf_manual(img, x = c(200, 780), y = c(130, 610))
#' dim(cr)
#' @export
crop_leaf_manual <- function(image, x, y) {
  image <- resolve_image(image)
  md <- attr(image, "leaf_metadata")
  d <- dim(image)
  if (length(x) < 2L || length(y) < 2L || length(x) != length(y) ||
      anyNA(x) || anyNA(y)) {
    leaf_abort("Manual crop requires at least two (x, y) points.")
  }
  bbox <- c(x_min = max(1, floor(min(x))), x_max = min(d[1], ceiling(max(x))),
            y_min = max(1, floor(min(y))), y_max = min(d[2], ceiling(max(y))))
  if (bbox["x_max"] - bbox["x_min"] < 2 || bbox["y_max"] - bbox["y_min"] < 2) {
    leaf_abort("Manual crop region is empty or too small.")
  }
  out <- crop_by_bbox(image, bbox)
  attr(out, "leaf_metadata") <- md
  attr(out, "crop_info") <- list(method = "manual", found = TRUE, bbox = bbox,
                                 original_dim = d[1:2])
  out
}

#' @noRd
crop_by_bbox <- function(image, bbox) {
  arr <- as_rgb_array(image)
  as_color_image(arr[bbox["x_min"]:bbox["x_max"], bbox["y_min"]:bbox["y_max"], , drop = FALSE])
}

#' Map a mask from a cropped image back to the original image size
#'
#' Needed, for instance, to compare a mask computed on a cropped image with a
#' reference (ground-truth) mask drawn on the full original photograph.
#'
#' @param mask Logical matrix computed on the cropped image.
#' @param crop_info The `crop_info` list (from [auto_crop_leaf()],
#'   [crop_leaf_manual()] or `result$crop`).
#' @return Logical matrix with the original image dimensions (`FALSE` outside
#'   the crop).
#' @examples
#' img <- read_leaf(leaf_example_images()[1])
#' res <- analyze_leaf(img, crop = "auto")
#' full <- uncrop_mask(res$leaf_mask, res$crop)
#' dim(full)
#' @export
uncrop_mask <- function(mask, crop_info) {
  mask <- check_mask(mask)
  if (is.null(crop_info) || identical(crop_info$method, "none")) return(mask)
  od <- crop_info$original_dim
  bb <- crop_info$bbox
  out <- matrix(FALSE, od[1], od[2])
  out[bb["x_min"]:bb["x_max"], bb["y_min"]:bb["y_max"]] <- mask
  out
}
