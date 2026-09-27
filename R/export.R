#' Export a single leaf analysis
#'
#' Writes the metrics (CSV), masks (PNG), overlay (PNG), the flattened
#' parameters (CSV) and the method report (TXT). CSV is the default tabular
#' format because it is open, interoperable and reproducible.
#'
#' Files written (with `prefix` = file stem by default):
#' `<prefix>_metrics.csv`, `<prefix>_parameters.csv`, `<prefix>_report.txt`,
#' `<prefix>_leaf_mask.png`, `<prefix>_healthy_mask.png`,
#' `<prefix>_injury_mask.png`, `<prefix>_class_map.png`,
#' `<prefix>_overlay.png` and, when normalisation was used,
#' `<prefix>_normalized.png`.
#'
#' @param x A `leaf_analysis` object.
#' @param output_dir Directory (created if needed).
#' @param prefix File-name prefix.
#' @return Character vector of written paths, invisibly.
#' @examples
#' res <- analyze_leaf(leaf_example_images()[1], crop = "auto")
#' files <- export_leaf_analysis(res, file.path(tempdir(), "leaf_export"))
#' basename(files)
#' @export
export_leaf_analysis <- function(x, output_dir, prefix = NULL) {
  if (!inherits(x, "leaf_analysis")) leaf_abort("`x` must be a 'leaf_analysis' object.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  prefix <- prefix %||% if (!is.na(x$metadata$filename %||% NA))
    tools::file_path_sans_ext(x$metadata$filename) else "leaf"
  p <- function(s) file.path(output_dir, paste0(prefix, s))
  files <- c(p("_metrics.csv"), p("_parameters.csv"), p("_report.txt"),
             p("_leaf_mask.png"), p("_healthy_mask.png"), p("_injury_mask.png"),
             p("_class_map.png"))
  utils::write.csv(x$metrics, files[1], row.names = FALSE)
  utils::write.csv(get_analysis_parameters(x, flatten = TRUE), files[2], row.names = FALSE)
  analysis_report(x, file = files[3])
  write_mask_png(x$leaf_mask, files[4])
  write_mask_png(x$healthy_mask, files[5])
  write_mask_png(x$injury_mask, files[6])
  write_image_png(class_map_to_image(x$class_map, x$parameters$multiclass), files[7])
  ov <- x$overlay %||% if (!is.null(x$cropped)) create_leaf_overlay(x) else NULL
  if (!is.null(ov)) { files <- c(files, p("_overlay.png")); write_image_png(ov, p("_overlay.png")) }
  if (!is.null(x$normalized)) {
    files <- c(files, p("_normalized.png")); write_image_png(x$normalized, p("_normalized.png"))
  }
  invisible(files)
}

#' Write tabular results to CSV
#'
#' @param x A `leaf_analysis`, `leaf_batch` or data frame of metrics.
#' @param file Output CSV path.
#' @return `file`, invisibly.
#' @examples
#' res <- analyze_leaf(leaf_example_images()[1])
#' write_leaf_results(res, tempfile(fileext = ".csv"))
#' @export
write_leaf_results <- function(x, file) {
  df <- if (inherits(x, "leaf_analysis")) x$metrics else if (inherits(x, "leaf_batch"))
    x$results else if (is.data.frame(x)) x else leaf_abort("Unsupported object.")
  utils::write.csv(df, file, row.names = FALSE)
  invisible(file)
}
