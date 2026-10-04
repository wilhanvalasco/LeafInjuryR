#' Benchmark the analysis pipeline
#'
#' Measures elapsed time per image (repeated `times` times) and an
#' approximate memory indicator: the maximum R heap used as reported by
#' [gc()] during the run (`max_mb`, reset before each image). Memory figures
#' are approximate and platform-dependent.
#'
#' Processing time grows roughly linearly with the number of pixels. For
#' routine work, images of about 1-4 megapixels with the leaf covering
#' 20-70% of the frame are a good compromise; larger images can be cropped
#' (`crop = "auto"`) or down-scaled before analysis (note that down-scaling
#' changes absolute pixel counts, but not percentages, to first order).
#'
#' @param images Image paths.
#' @param times Repetitions per image.
#' @param ... Passed to [analyze_leaf()].
#' @return A data frame with `file`, `megapixels`, `run`, `elapsed_sec`,
#'   `max_mb`.
#' @noRd
benchmark_leaf_analysis <- function(images, times = 1L, ...) {
  rows <- list()
  for (f in images) {
    for (k in seq_len(times)) {
      invisible(gc(reset = TRUE))
      t <- system.time(res <- analyze_leaf(f, keep_images = FALSE, ...))[["elapsed"]]
      g <- gc()
      mb <- sum(g[, ncol(g)])
      rows[[length(rows) + 1L]] <- data.frame(
        file = basename(f),
        megapixels = res$metrics$image_width * res$metrics$image_height / 1e6,
        run = k, elapsed_sec = t, max_mb = mb, stringsAsFactors = FALSE)
      rm(res)
    }
  }
  do.call(rbind, rows)
}
