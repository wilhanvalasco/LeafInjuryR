#' Analyse many leaf images
#'
#' Applies [analyze_leaf()] to each image with the same settings. Errors are
#' handled per image: a failing image is recorded with `status = "failed"`
#' and its error message, and the batch continues. Images are not kept in
#' memory; if `output_dir` is given, results are written as they are
#' produced:
#'
#' ```
#' output_dir/
#'   results.csv
#'   parameters.csv
#'   analysis_report.txt
#'   masks/     <name>_leaf_mask.png, <name>_injury_mask.png, <name>_class_map.png
#'   overlays/  <name>_overlay.png
#' ```
#'
#' Processing is sequential in version 0.1.0; the per-image function is
#' self-contained so parallel back-ends can be added later.
#'
#' @param images Character vector of image paths, or a single directory.
#' @param output_dir Optional output directory (created if needed).
#' @param save_masks,save_overlays Logical; write PNG files (requires
#'   `output_dir`).
#' @param progress Logical; show a text progress bar.
#' @param callback Optional function `function(i, n, file, status)` called
#'   after each image (used by the Shiny interface).
#' @param recursive When `images` is a directory, search sub-directories.
#' @param ... Arguments passed to [analyze_leaf()] (e.g. `method`, `crop`,
#'   `multiclass`, `config`).
#' @return An object of class `leaf_batch`: list with `results` (data frame,
#'   one row per image, including `status`, `error_message` and
#'   `elapsed_sec`), `parameters` (settings shared by all images),
#'   `output_dir`, `files` and `total_elapsed_sec`.
#' @examples
#' out <- file.path(tempdir(), "leaf_batch_example")
#' b <- analyze_leaf_batch(leaf_example_images(), output_dir = out, crop = "auto")
#' b$results[, c("file", "healthy_percent", "injured_percent", "quality_flag", "status")]
#' list.files(out, recursive = TRUE)
#' @export
analyze_leaf_batch <- function(images, output_dir = NULL, save_masks = TRUE,
                               save_overlays = TRUE, progress = interactive(),
                               callback = NULL, recursive = FALSE, ...) {
  t0 <- Sys.time()
  if (length(images) == 1L && dir.exists(images)) images <- list_leaf_images(images, recursive)
  if (!length(images)) leaf_abort("No images to process.")
  args <- list(...)
  if (!is.null(args$keep_images)) args$keep_images <- NULL
  if (!is.null(args$display)) args$display <- NULL
  write_out <- !is.null(output_dir)
  if (write_out) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (save_masks) dir.create(file.path(output_dir, "masks"), showWarnings = FALSE)
    if (save_overlays) dir.create(file.path(output_dir, "overlays"), showWarnings = FALSE)
  }
  stems <- make.unique(tools::file_path_sans_ext(basename(images)), sep = "_")
  n <- length(images)
  pb <- if (isTRUE(progress)) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
  rows <- vector("list", n)
  first_params <- NULL
  for (i in seq_len(n)) {
    ti <- Sys.time()
    row <- tryCatch({
      keep <- write_out && save_overlays
      res <- do.call(analyze_leaf, c(list(image = images[i], keep_images = keep), args))
      if (is.null(first_params)) first_params <- res$parameters
      if (write_out && save_masks) {
        write_mask_png(res$leaf_mask, file.path(output_dir, "masks", paste0(stems[i], "_leaf_mask.png")))
        write_mask_png(res$injury_mask, file.path(output_dir, "masks", paste0(stems[i], "_injury_mask.png")))
        write_image_png(class_map_to_image(res$class_map, res$parameters$multiclass),
                        file.path(output_dir, "masks", paste0(stems[i], "_class_map.png")))
      }
      if (write_out && save_overlays) {
        write_image_png(res$overlay, file.path(output_dir, "overlays", paste0(stems[i], "_overlay.png")))
      }
      m <- res$metrics
      m$file <- basename(images[i])
      m$quality_messages <- paste(res$quality$quality_messages, collapse = " | ")
      m$status <- "ok"
      m$error_message <- NA_character_
      rm(res)
      m
    }, error = function(e) {
      data.frame(file = basename(images[i]), status = "failed",
                 error_message = conditionMessage(e), quality_flag = "failed",
                 stringsAsFactors = FALSE)
    })
    row$output_stem <- stems[i]
    row$elapsed_sec <- as.numeric(difftime(Sys.time(), ti, units = "secs"))
    rows[[i]] <- row
    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    if (is.function(callback)) callback(i, n, basename(images[i]), row$status)
  }
  if (!is.null(pb)) close(pb)
  results <- bind_rows_fill(rows)
  front <- c("file", "leaf_pixels", "healthy_pixels", "injured_pixels", "healthy_percent",
             "injured_percent", "quality_flag", "method", "status")
  results <- results[c(intersect(front, names(results)), setdiff(names(results), front))]
  params <- first_params %||% list(package_version = pkg_version("LeafInjuryR"))
  params$file <- NULL
  params$analysis_timestamp <- format(t0, "%Y-%m-%dT%H:%M:%S%z")
  out <- structure(list(results = results, parameters = params, output_dir = output_dir,
                        files = images,
                        total_elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs"))),
                   class = "leaf_batch")
  if (write_out) {
    utils::write.csv(results, file.path(output_dir, "results.csv"), row.names = FALSE)
    if (!is.null(first_params)) {
      utils::write.csv(get_analysis_parameters(out, flatten = TRUE),
                       file.path(output_dir, "parameters.csv"), row.names = FALSE)
      analysis_report(out, file = file.path(output_dir, "analysis_report.txt"))
    }
  }
  out
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
  if (!is.null(x$output_dir)) cat("Output directory:", x$output_dir, "\n")
  invisible(x)
}

#' @export
as.data.frame.leaf_batch <- function(x, ...) x$results
