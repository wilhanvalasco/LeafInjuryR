#' Crop a leaf image automatically or manually
#'
#' Crops the photograph to the region of interest **without resampling**:
#' the original resolution and aspect of every pixel are preserved, and the
#' original image object is never modified.
#'
#' * `mode = "auto"` locates the main leaf on a down-scaled preview with the
#'   same segmentation used for analysis, takes its bounding box and adds a
#'   margin (`margin`, default 5% of the box) so that leaf borders are kept.
#'   If no leaf is found the full image is returned and
#'   `attr(x, "crop_info")$found` is `FALSE`.
#' * `mode = "manual"` crops to the bounding box of the points `x`, `y`
#'   (pixel coordinates; x along the width, y along the height, origin at the
#'   top-left). In the Shiny app these come from a rectangle drawn with the
#'   mouse. Coordinates outside the image are clipped.
#'
#' @param image Path to an image file (JPEG, PNG, TIFF) or an EBImage `Image`.
#' @param mode `"auto"` or `"manual"`.
#' @param x,y Numeric pixel coordinates (at least two points) for manual crop.
#' @param margin Margin around the automatic bounding box, as a fraction of its
#'   size.
#' @param method Segmentation method used to locate the leaf in automatic mode
#'   (see [segment_leaf()]).
#' @param params Optional parameter overrides (see [analyze_leaf()]).
#' @return The cropped `Image`. Attribute `crop_info` records `method`,
#'   `found`, `bbox` (`x_min`, `x_max`, `y_min`, `y_max`, in original pixel
#'   coordinates) and `original_dim`, so masks can be mapped back to the
#'   original photograph.
#' @examples
#' f <- system.file("extdata", "leaf_example_01.jpg", package = "LeafInjuryR")
#' auto <- crop_leaf(f)
#' attr(auto, "crop_info")$bbox
#' man <- crop_leaf(f, mode = "manual", x = c(200, 780), y = c(130, 610))
#' dim(man)
#' @export
crop_leaf <- function(image, mode = c("auto", "manual"), x = NULL, y = NULL,
                      margin = NULL, method = "auto", params = list()) {
  mode <- match.arg(mode)
  config <- params_to_config(params)
  if (mode == "auto") {
    auto_crop_leaf(image, method = method, margin = margin, config = config)
  } else {
    if (is.null(x) || is.null(y)) leaf_abort("Manual crop requires `x` and `y` coordinates.")
    crop_leaf_manual(image, x, y)
  }
}
