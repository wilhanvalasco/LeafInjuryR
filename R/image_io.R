#' Supported image file extensions
#' @noRd
supported_extensions <- function() c("jpg", "jpeg", "png", "tif", "tiff")

#' Read a leaf image
#'
#' Reads a JPEG, PNG or TIFF file into an [EBImage::Image] object with values
#' in \[0, 1\]. Alpha channels are dropped. Basic metadata (file name,
#' extension, width, height, channels) are attached as the attribute
#' `"leaf_metadata"`. The file is read locally; nothing is transmitted.
#'
#' EBImage stores images as `[width, height, channel]` arrays: the first index
#' is the x (column) coordinate and the second the y (row) coordinate,
#' counted from the top-left corner.
#'
#' @param path Path to an image file.
#' @return An `Image` (colour mode) with attribute `leaf_metadata`.
#' @noRd
read_leaf <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    leaf_abort("`path` must be a single file path.", "leafinjury_read_error")
  }
  if (!file.exists(path)) {
    leaf_abort(sprintf("Image file could not be read: '%s' does not exist.", basename(path)),
               "leafinjury_read_error")
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% supported_extensions()) {
    leaf_abort(sprintf("Unsupported image format: '.%s'. Supported: %s.", ext,
                       paste(supported_extensions(), collapse = ", ")),
               "leafinjury_unsupported_format")
  }
  img <- tryCatch(suppressWarnings(EBImage::readImage(path)),
                  error = function(e) NULL)
  if (is.null(img)) {
    leaf_abort("Image file could not be read.", "leafinjury_read_error")
  }
  d <- dim(img)
  channels <- if (length(d) >= 3L) d[3] else 1L
  if (length(d) > 3L) {
    leaf_abort("Image file could not be read: multi-frame images are not supported.",
               "leafinjury_read_error")
  }
  arr <- as_rgb_array(img)  # errors on grayscale / incompatible channels
  out <- as_color_image(arr)
  attr(out, "leaf_metadata") <- list(
    filename = basename(path),
    extension = ext,
    width = d[1],
    height = d[2],
    channels = channels
  )
  out
}

#' Metadata of a leaf image
#'
#' @param image An image returned by [read_leaf()] or any EBImage `Image`.
#' @return A list with `filename`, `extension`, `width`, `height`, `channels`
#'   (`NA` where unknown).
#' @noRd
leaf_image_metadata <- function(image) {
  md <- attr(image, "leaf_metadata")
  d <- dim(image)
  if (is.null(md)) {
    md <- list(filename = NA_character_, extension = NA_character_,
               width = d[1], height = d[2],
               channels = if (length(d) >= 3L) d[3] else 1L)
  }
  md
}

#' Coerce an input (path or image) to an RGB Image, keeping metadata
#' @noRd
resolve_image <- function(image) {
  if (is.character(image)) return(read_leaf(image))
  md <- attr(image, "leaf_metadata")
  arr <- as_rgb_array(image)
  out <- as_color_image(arr)
  attr(out, "leaf_metadata") <- md %||% list(
    filename = NA_character_, extension = NA_character_,
    width = dim(arr)[1], height = dim(arr)[2], channels = 3L)
  out
}

#' Write a logical mask or class map to PNG
#'
#' @param mask Logical matrix `[width, height]`.
#' @param path Output file path (`.png`).
#' @return `path`, invisibly.
#' @noRd
write_mask_png <- function(mask, path) {
  mask <- check_mask(mask)
  EBImage::writeImage(EBImage::Image(mask * 1, colormode = EBImage::Grayscale),
                      path, type = "png")
  invisible(path)
}

#' Write a colour image to PNG
#' @param image Colour `Image` or array `[width, height, 3]`.
#' @param path Output file path.
#' @return `path`, invisibly.
#' @noRd
write_image_png <- function(image, path) {
  EBImage::writeImage(as_color_image(as_rgb_array(image)), path, type = "png")
  invisible(path)
}

#' List image files in a directory
#'
#' @param dir Directory.
#' @param recursive Search sub-directories.
#' @return Character vector of image paths with supported extensions.
#' @noRd
list_leaf_images <- function(dir, recursive = FALSE) {
  if (!dir.exists(dir)) leaf_abort(sprintf("Directory not found: '%s'.", dir))
  pattern <- paste0("\\.(", paste(supported_extensions(), collapse = "|"), ")$")
  files <- list.files(dir, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  files[!grepl("(^|/)__MACOSX/|(^|/)\\._", files)]
}
