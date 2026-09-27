#' Launch the LeafInjuryR Shiny interface
#'
#' Opens a local graphical interface (single-image analysis with automatic or
#' manual cropping, batch processing, method comparison and documentation).
#' Everything runs on the local machine: there is no login, no telemetry and
#' no image is uploaded to any external server ("upload" in the interface
#' means reading a file from your own computer into the local R session).
#'
#' Requires the suggested packages \pkg{shiny}, \pkg{bslib} and \pkg{DT}.
#'
#' @param launch.browser Passed to [shiny::runApp()].
#' @param port Optional port.
#' @param max_upload_mb Maximum upload size in megabytes.
#' @return Called for its side effect (runs the app).
#' @examples
#' if (interactive()) run_leafinjury_app()
#' @export
run_leafinjury_app <- function(launch.browser = TRUE, port = NULL, max_upload_mb = 200) {
  needed <- c("shiny", "bslib", "DT")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    leaf_abort(sprintf("The Shiny interface needs package(s): %s. Install with install.packages(c(%s)).",
                       paste(missing, collapse = ", "),
                       paste0('"', missing, '"', collapse = ", ")))
  }
  app_dir <- system.file("shiny", "app", package = "LeafInjuryR")
  if (app_dir == "") leaf_abort("Shiny app directory not found; reinstall LeafInjuryR.")
  old <- options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(old), add = TRUE)
  args <- list(appDir = app_dir, launch.browser = launch.browser)
  if (!is.null(port)) args$port <- port
  do.call(shiny::runApp, args)
}
