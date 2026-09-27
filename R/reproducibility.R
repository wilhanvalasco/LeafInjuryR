#' Retrieve the parameters used in an analysis
#'
#' Every `leaf_analysis` stores the package, R and EBImage versions, a
#' timestamp, the segmentation and tissue methods, normalisation, cropping,
#' image-derived and user thresholds, morphological and quality-control
#' parameters, the random seed and the full [leaf_config()]. The same file,
#' parameters and package version reproduce the same result (all sampling uses
#' the stored seed).
#'
#' @param x A `leaf_analysis` or `leaf_batch` object.
#' @param flatten If `TRUE`, return a two-column data frame (`parameter`,
#'   `value`) suitable for CSV export.
#' @return A nested list, or a data frame when `flatten = TRUE`.
#' @examples
#' res <- analyze_leaf(leaf_example_images()[1])
#' p <- get_analysis_parameters(res)
#' p$background_strategy
#' head(get_analysis_parameters(res, flatten = TRUE))
#' @export
get_analysis_parameters <- function(x, flatten = FALSE) {
  p <- if (inherits(x, "leaf_analysis") || inherits(x, "leaf_batch")) x$parameters else
    leaf_abort("`x` must be a 'leaf_analysis' or 'leaf_batch' object.")
  if (!flatten) return(p)
  flatten_list(p)
}

#' @noRd
flatten_list <- function(x, prefix = NULL) {
  rows <- list()
  add <- function(name, value) {
    rows[[length(rows) + 1L]] <<- data.frame(parameter = name, value = value,
                                             stringsAsFactors = FALSE)
  }
  walk <- function(obj, name) {
    if (is.data.frame(obj)) {
      add(name, if (nrow(obj)) paste(utils::capture.output(print(obj, row.names = FALSE)),
                                    collapse = " | ") else "")
    } else if (is.list(obj)) {
      if (!length(obj)) add(name, "")
      nms <- names(obj) %||% as.character(seq_along(obj))
      for (i in seq_along(obj)) walk(obj[[i]], if (is.null(name)) nms[i] else paste(name, nms[i], sep = "."))
    } else if (is.matrix(obj)) {
      add(name, paste(signif(obj, 6), collapse = ", "))
    } else {
      v <- if (is.null(obj)) "NULL" else if (is.numeric(obj)) {
        paste0(if (!is.null(names(obj))) paste0(names(obj), "=") else "",
               signif(obj, 8), collapse = ", ")
      } else paste(obj, collapse = ", ")
      add(name, if (length(v)) v else "")
    }
  }
  walk(x, prefix)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Generate a method report for an analysis
#'
#' Produces a plain-text summary of the methods and parameters used, intended
#' to help writing the *Material and Methods* section of a scientific report.
#' It describes *what was done*, and makes no claim of scientific validity.
#'
#' @param x A `leaf_analysis` or `leaf_batch` object.
#' @param file Optional path; if given, the report is written to this file.
#' @return A character vector of class `leaf_report` (printed nicely).
#' @examples
#' res <- analyze_leaf(leaf_example_images()[1], crop = "auto")
#' analysis_report(res)
#' @export
analysis_report <- function(x, file = NULL) {
  p <- get_analysis_parameters(x)
  seg_desc <- c(
    auto = "adaptive (Otsu thresholds on CIELAB chroma and darkness, background polarity inferred from image border, union of accepted features; k-means fallback)",
    lab = "Otsu threshold on CIELAB chroma", hsv = "Otsu threshold on HSV saturation",
    exg = "Otsu threshold on excess green (ExG)", otsu = "Otsu threshold on grey intensity",
    adaptive = "Otsu threshold on lightness after polynomial background-surface removal",
    kmeans = "k-means clustering on standardised CIELAB; border-dominant clusters = background")
  tis_desc <- c(
    auto = "CIELAB hue angle with within-leaf Otsu threshold constrained to configured bounds",
    lab = "CIELAB hue-angle rule", hsv = "HSV hue/saturation rule",
    exgr = "ExG - ExR index", vote = "majority vote of CIELAB, HSV and ExG-ExR rules")
  fmt_thr <- function(l) {
    if (!length(l)) return("none")
    paste(vapply(names(l), function(n) paste0(n, " = ", paste(signif(l[[n]], 4), collapse = "-")),
                 character(1)), collapse = "; ")
  }
  mo <- p$morphological_parameters
  lines <- c(
    "LeafInjuryR analysis report",
    "===========================",
    sprintf("Package version: %s (R %s, EBImage %s)", p$package_version, p$R_version,
            p$EBImage_version),
    sprintf("Analysis timestamp: %s", p$analysis_timestamp),
    if (inherits(x, "leaf_batch")) sprintf("Images processed: %d (failed: %d)",
                                           nrow(x$results), sum(x$results$status != "ok"))
    else sprintf("File: %s", p$file),
    "",
    sprintf("Automatic crop: %s", switch(p$crop_method, none = "disabled",
                                         auto = "enabled (bounding box of main leaf + margin)",
                                         manual = "manual rectangle")),
    sprintf("Normalization: %s", if (isTRUE(p$normalization$enabled))
      paste("enabled -", paste(p$normalization$applied, collapse = " + ")) else "disabled"),
    sprintf("Segmentation method: %s - %s", p$background_method,
            seg_desc[[p$background_method]]),
    if (!inherits(x, "leaf_batch"))
      sprintf("Segmentation strategy/thresholds used: %s; %s (%s)", p$background_strategy,
              fmt_thr(p$thresholds$segmentation), p$thresholds$segmentation_source),
    sprintf("Morphology: opening radius %s px (%s), minimum object size %s, keep largest component: %s, hole policy: %s",
            mo$opening_radius, mo$brush_shape,
            if (!is.null(mo$min_object_size_used)) paste(mo$min_object_size_used, "px")
            else if (!is.null(mo$min_object_size)) paste(mo$min_object_size, "px")
            else paste0(mo$min_object_fraction * 100, "% of image area"),
            mo$keep_largest, if (isTRUE(mo$fill_holes)) mo$hole_policy else "none"),
    sprintf("Tissue classification: %s - %s", p$tissue_method, tis_desc[[p$tissue_method]]),
    if (!inherits(x, "leaf_batch"))
      sprintf("Tissue thresholds used: %s", fmt_thr(p$thresholds$tissue)),
    sprintf("Classes: %s", if (isTRUE(p$multiclass))
      "healthy, chlorotic, necrotic, other (injured = chlorotic + necrotic + other)"
      else "healthy, injured"),
    "Injury percentage denominator: leaf-mask pixels only (background excluded).",
    sprintf("Random seed: %s", p$seed),
    "",
    "Note: results quantify visual colour patterns. The method has not been validated",
    "against manual reference masks by this report; see vignette('method-validation').",
    "Colour changes may have different biological causes; no diagnosis is implied."
  )
  lines <- lines[!vapply(lines, is.null, logical(1))]
  lines <- unlist(lines)
  if (!is.null(file)) writeLines(lines, file)
  structure(lines, class = c("leaf_report", "character"))
}

#' @export
print.leaf_report <- function(x, ...) {
  writeLines(unclass(x))
  invisible(x)
}
