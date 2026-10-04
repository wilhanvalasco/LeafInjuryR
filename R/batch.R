#' Analyse many leaf images with the same settings
#'
#' Applies [analyze_leaf()] to every image with identical parameters. Each
#' image is processed independently: an image that fails is recorded with
#' `status = "failed"` and its error message, and the batch continues.
#' Images are released from memory after each analysis; only small
#' thumbnails are kept for the reports.
#'
#' When `output_dir` is given, the three reports are generated automatically
#' from the same results table (consistent by construction):
#'
#' ```
#' output_dir/
#'   report.html     self-contained interactive report (opens offline)
#'   results.xlsx    sheets "Resultados", "Resumo", "Parametros"
#'   results.csv     one row per image, one column per variable
#'   masks/          <name>_leaf_mask.png, <name>_injury_mask.png, <name>_class_map.png
#'   overlays/       <name>_overlay.png
#' ```
#'
#' Batch analysis uses automatic segmentation only; manual corrections are
#' made image by image with [analyze_leaf()] or in the Shiny app.
#'
#' @param images Character vector of image paths, or a single directory.
#' @param output_dir Optional output directory (created if needed).
#' @param reports Report formats to write: any of `"html"`, `"xlsx"`, `"csv"`.
#' @param save_images Write mask and overlay PNG files (requires `output_dir`).
#' @param progress Show a text progress bar.
#' @param callback Optional `function(i, n, file, status)` called after each
#'   image (used by the Shiny app).
#' @param recursive When `images` is a directory, search sub-directories.
#' @param ... Settings passed to [analyze_leaf()] (e.g. `crop`, `method`,
#'   `tissue_method`, `multiclass`, `normalize`, `pixels_per_cm`, `params`).
#' @return An object of class `leaf_batch`: `results` (data frame), `summary`
#'   (descriptive statistics), `parameters`, `settings`, `thumbnails`,
#'   `output_dir`, `report_files`, `files` and `total_elapsed_sec`.
#' @examples
#' imgs <- system.file("extdata", c("leaf_example_01.jpg", "leaf_example_02.jpg"),
#'                     package = "LeafInjuryR")
#' out <- file.path(tempdir(), "leaf_batch_example")
#' b <- analyze_leaf_batch(imgs, output_dir = out, crop = "auto", progress = FALSE)
#' b
#' list.files(out)
#' @export
analyze_leaf_batch <- function(images, output_dir = NULL, reports = c("html", "xlsx", "csv"),
                               save_images = TRUE, progress = interactive(), callback = NULL,
                               recursive = FALSE, ...) {
  t0 <- Sys.time()
  if (length(images) == 1L && dir.exists(images)) images <- list_leaf_images(images, recursive)
  if (!length(images)) leaf_abort("No images to process.")
  reports <- if (length(reports)) match.arg(reports, c("html", "xlsx", "csv"), several.ok = TRUE) else character(0)
  args <- list(...)
  bad <- intersect(names(args), c("segmentation", "corrections", "keep_images", "display", "image"))
  if (length(bad)) leaf_abort(sprintf("Argument(s) not used in batch mode: %s.", paste(bad, collapse = ", ")))
  write_out <- !is.null(output_dir)
  if (write_out) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (save_images) {
      dir.create(file.path(output_dir, "masks"), showWarnings = FALSE)
      dir.create(file.path(output_dir, "overlays"), showWarnings = FALSE)
    }
  }
  stems <- make.unique(tools::file_path_sans_ext(basename(images)), sep = "_")
  n <- length(images)
  pb <- if (isTRUE(progress)) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
  rows <- vector("list", n); thumbs <- list(); first_params <- NULL
  for (i in seq_len(n)) {
    ti <- Sys.time()
    row <- tryCatch({
      res <- do.call(analyze_leaf, c(list(image = images[i], keep_images = TRUE), args))
      if (is.null(first_params)) first_params <- res$parameters
      thumbs[[stems[i]]] <- list(original = image_data_uri(res$cropped),
                                 processed = image_data_uri(res$overlay))
      if (write_out && save_images) save_analysis_images(res, output_dir, stems[i])
      m <- res$metrics
      m$file <- basename(images[i])
      m$quality_messages <- paste(res$quality$quality_messages, collapse = " | ")
      m$status <- "ok"; m$error_message <- NA_character_
      rm(res)
      m
    }, error = function(e) {
      data.frame(file = basename(images[i]), status = "failed",
                 error_message = conditionMessage(e), quality_flag = "failed",
                 file_md5 = if (file.exists(images[i])) unname(tools::md5sum(images[i])) else NA_character_,
                 stringsAsFactors = FALSE)
    })
    row$output_stem <- stems[i]
    row$elapsed_sec <- as.numeric(difftime(Sys.time(), ti, units = "secs"))
    rows[[i]] <- row
    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    if (is.function(callback)) callback(i, n, basename(images[i]), row$status)
  }
  if (!is.null(pb)) close(pb)
  results <- order_result_columns(bind_rows_fill(rows))
  params <- first_params %||% list(package_version = pkg_version("LeafInjuryR"))
  params[c("file", "file_md5", "manual_corrections")] <- NULL
  params$analysis_timestamp <- format(t0, "%Y-%m-%dT%H:%M:%S%z")
  out <- structure(list(
    results = results, summary = batch_summary(results), parameters = params,
    settings = settings_record(args), thumbnails = thumbs, output_dir = output_dir,
    report_files = character(0), files = images,
    total_elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs"))),
    class = "leaf_batch")
  if (write_out && length(reports)) out$report_files <- write_leaf_reports(out, output_dir, reports)
  out
}

#' Record of the settings passed through `...` (no large objects)
#' @noRd
settings_record <- function(args) {
  keep <- vapply(args, function(a) is.atomic(a) && length(a) <= 20, logical(1))
  out <- args[keep]
  if (!is.null(args$params)) out$params <- "see parameters$config"
  out
}

#' @noRd
save_analysis_images <- function(res, output_dir, stem) {
  write_mask_png(res$leaf_mask, file.path(output_dir, "masks", paste0(stem, "_leaf_mask.png")))
  write_mask_png(res$injury_mask, file.path(output_dir, "masks", paste0(stem, "_injury_mask.png")))
  write_image_png(class_map_to_image(res$class_map, res$parameters$multiclass),
                  file.path(output_dir, "masks", paste0(stem, "_class_map.png")))
  if (!is.null(res$overlay))
    write_image_png(res$overlay, file.path(output_dir, "overlays", paste0(stem, "_overlay.png")))
}

#' @noRd
order_result_columns <- function(results) {
  front <- c("file", "status", "leaf_pixels", "healthy_pixels", "injured_pixels",
             "healthy_percent", "injured_percent", "chlorotic_pixels", "necrotic_pixels",
             "other_pixels", "chlorotic_percent", "necrotic_percent", "other_percent",
             "leaf_area_cm2", "healthy_area_cm2", "injured_area_cm2", "quality_flag",
             "quality_messages", "error_message", "method", "tissue_method", "multiclass")
  results[c(intersect(front, names(results)), setdiff(names(results), front))]
}

#' @noRd
bind_rows_fill <- function(rows) {
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(df) {
    for (cc in setdiff(cols, names(df))) df[[cc]] <- NA
    df[cols]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @export
print.leaf_batch <- function(x, ...) {
  r <- x$results
  cat("<leaf_batch>", nrow(r), "image(s):", sum(r$status == "ok"), "ok,",
      sum(r$status != "ok"), "failed\n")
  show <- intersect(c("file", "healthy_percent", "injured_percent", "quality_flag", "status"), names(r))
  print(utils::head(r[show], 10), row.names = FALSE)
  if (length(x$report_files)) cat("Reports:", paste(basename(x$report_files), collapse = ", "), "\n")
  if (!is.null(x$output_dir)) cat("Output directory:", x$output_dir, "\n")
  invisible(x)
}

#' @export
as.data.frame.leaf_batch <- function(x, ...) x$results
