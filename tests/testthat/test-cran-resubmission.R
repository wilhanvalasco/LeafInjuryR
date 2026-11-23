# Regression tests for the changes requested in the CRAN review of 0.2.1 (fixed in 0.2.2):
# (1) the validate_leaf() example no longer needs the triple-colon operator; (2) global state
# (par, options, working directory, RNG) is restored; (3) the Shiny app still
# loads and its helpers restore state; (4) results are unchanged.

uncrop_public <- function(mask, crop) {
  # Same reconstruction as in the validate_leaf() example (public fields only)
  bb <- crop$bbox; od <- crop$original_dim
  out <- matrix(FALSE, nrow = od[1], ncol = od[2])
  out[bb["x_min"]:bb["x_max"], bb["y_min"]:bb["y_max"]] <- mask
  out
}

test_that("mask reconstruction from the public crop record equals uncrop_mask()", {
  f <- system.file("extdata", "leaf_example_01.jpg", package = "LeafInjuryR")
  res <- analyze_leaf(f, crop = "auto")
  expect_true(isTRUE(res$crop$found))
  expect_identical(uncrop_public(res$leaf_mask, res$crop), uncrop_mask(res$leaf_mask, res$crop))
  expect_identical(uncrop_public(res$injury_mask, res$crop), uncrop_mask(res$injury_mask, res$crop))
  # manual crop as well
  crp <- crop_leaf(f, mode = "manual", x = c(200, 780), y = c(130, 610))
  res_m <- analyze_leaf(crp)
  info <- attr(crp, "crop_info")
  expect_identical(uncrop_public(res_m$leaf_mask, info), uncrop_mask(res_m$leaf_mask, info))
})

test_that("validation metrics are identical with matrix or PNG references (example path)", {
  f <- system.file("extdata", "leaf_example_01.jpg", package = "LeafInjuryR")
  res <- analyze_leaf(f, crop = "auto")
  ref_mask <- uncrop_public(res$leaf_mask, res$crop)
  m_matrix <- validate_leaf(res, reference_leaf_mask = ref_mask)$metrics
  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  EBImage::writeImage(EBImage::Image(1 * ref_mask), png)
  m_png <- validate_leaf(res, reference_leaf_mask = png)$metrics
  expect_equal(m_matrix, m_png)
  expect_equal(m_matrix$iou, 1)
  expect_equal(m_matrix$dice, 1)
  expect_equal(m_matrix$tp, sum(res$leaf_mask))
  expect_equal(m_matrix$fp, 0)
  expect_equal(m_matrix$fn, 0)
})

test_that("IoU and Dice agree with their closed-form definitions on a perturbed reference", {
  f <- system.file("extdata", "leaf_example_02.jpg", package = "LeafInjuryR")
  res <- analyze_leaf(f, crop = "auto")
  pred <- uncrop_public(res$leaf_mask, res$crop)
  ref <- pred
  cols <- which(colSums(ref) > 0)
  ref[, head(cols, 20)] <- FALSE           # deterministic perturbation
  m <- validate_leaf(res, reference_leaf_mask = ref)$metrics
  tp <- sum(pred & ref); fp <- sum(pred & !ref); fn <- sum(!pred & ref)
  expect_equal(c(m$tp, m$fp, m$fn), c(tp, fp, fn))
  expect_equal(m$iou, tp / (tp + fp + fn))
  expect_equal(m$dice, 2 * tp / (2 * tp + fp + fn))
  expect_equal(m$precision, tp / (tp + fp))
  expect_equal(m$sensitivity, tp / (tp + fn))
})

test_that("classification of the example images is deterministic", {
  f <- system.file("extdata", "leaf_example_02.jpg", package = "LeafInjuryR")
  a <- analyze_leaf(f, crop = "auto", multiclass = TRUE)
  b <- analyze_leaf(f, crop = "auto", multiclass = TRUE)
  expect_identical(a$class_map, b$class_map)
  expect_identical(a$leaf_mask, b$leaf_mask)
  expect_equal(a$metrics$injured_percent, b$metrics$injured_percent)
})

test_that("plot methods restore graphical parameters", {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 800, height = 600)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  syn <- make_synthetic_leaf(chlorotic = TRUE)
  res <- analyze_leaf(syn$image)
  v <- validate_leaf(res, reference_leaf_mask = syn$leaf_mask,
                     reference_injury_mask = syn$injury_mask)
  seg <- segment_leaf(syn$image)
  before <- graphics::par(no.readonly = TRUE)
  plot(v); expect_identical(graphics::par(no.readonly = TRUE)[c("mfrow", "oma", "mar", "fig")],
                            before[c("mfrow", "oma", "mar", "fig")])
  plot(res); expect_identical(graphics::par(no.readonly = TRUE)[c("mfrow", "mar")],
                              before[c("mfrow", "mar")])
  plot(seg); expect_identical(graphics::par(no.readonly = TRUE)[c("mfrow", "mar")],
                              before[c("mfrow", "mar")])
  plot(validate_leaf(res)); expect_identical(graphics::par("mfrow"), before$mfrow)
})

test_that("internal helpers restore the RNG state and the working directory", {
  set.seed(42); state <- .Random.seed
  invisible(make_synthetic_leaf(noise = 0.05, seed = 7))
  expect_identical(.Random.seed, state)
  # same image as before (noise generated with seed 7)
  expect_identical(make_synthetic_leaf(noise = 0.05, seed = 7)$image,
                   make_synthetic_leaf(noise = 0.05, seed = 7)$image)

  skip_if(Sys.which("zip") == "", "no zip utility")
  d <- tempfile("zipsrc_"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  writeLines("a", file.path(d, "a.txt"))
  wd <- getwd()
  z <- tempfile(fileext = ".zip")
  zip_files(z, file.path(d, "a.txt"))
  expect_identical(getwd(), wd)
  expect_true(file.exists(z))
  unlink(z)
})

test_that("the validation vignette's synthetic image equals the internal generator", {
  # vignettes/method-validation.Rmd builds the image with public code only
  width <- 120; height <- 90
  x <- matrix(seq_len(width), width, height)
  y <- matrix(seq_len(height), width, height, byrow = TRUE)
  cx <- width / 2; cy <- height / 2
  leaf_mask <- ((x - cx) / (width * 0.35))^2 + ((y - cy) / (height * 0.35))^2 <= 1
  necrotic <- leaf_mask & ((x - cx * 0.8)^2 + (y - cy)^2 <= 8^2)
  chlorotic <- leaf_mask & !necrotic & x > cx * 1.25 & x <= cx * 1.4
  colors <- list(background = c(0.94, 0.94, 0.93), healthy = c(0.30, 0.60, 0.12),
                 necrotic = c(0.42, 0.25, 0.10), chlorotic = c(0.85, 0.72, 0.20))
  arr <- array(0, c(width, height, 3))
  for (ch in 1:3) {
    layer <- matrix(colors$background[ch], width, height)
    layer[leaf_mask] <- colors$healthy[ch]
    layer[chlorotic] <- colors$chlorotic[ch]
    layer[necrotic] <- colors$necrotic[ch]
    arr[, , ch] <- layer
  }
  set.seed(1)
  arr <- pmin(pmax(arr + stats::rnorm(length(arr), 0, 0.01), 0), 1)
  img <- EBImage::Image(arr, colormode = EBImage::Color)
  syn <- make_synthetic_leaf(chlorotic = TRUE)
  expect_equal(EBImage::imageData(img), EBImage::imageData(syn$image))
  expect_identical(leaf_mask, syn$leaf_mask)
  expect_identical(necrotic | chlorotic, syn$injury_mask)
})

triple_colon <- strrep(":", 3)

test_that("installed documentation does not use the triple-colon operator", {
  db <- tools::Rd_db("LeafInjuryR")
  skip_if(length(db) == 0, "Rd database not available")
  txt <- vapply(db, function(rd) paste(as.character(rd), collapse = ""), character(1))
  hits <- grepl(triple_colon, txt, fixed = TRUE)
  expect_false(any(hits), info = paste(names(txt)[hits], collapse = ", "))
})

test_that("Shiny app: no triple-colon calls and helpers restore par and working directory", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  app <- system.file("shiny", "app", package = "LeafInjuryR")
  files <- c(file.path(app, "app.R"), list.files(file.path(app, "modules"), full.names = TRUE))
  code <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE); l[!grepl("^\\s*#", l)]
  }))
  expect_false(any(grepl(triple_colon, code, fixed = TRUE)))
  expect_false(any(grepl("(^|[^.[:alnum:]_])options\\(", code)))

  env <- new.env(parent = globalenv())
  sys.source(file.path(app, "modules", "helpers.R"), envir = env)

  # with_par(): restored after success and after an error
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  mar0 <- graphics::par("mar")
  env$with_par(list(mar = c(0, 0, 0, 0)), graphics::plot.new())
  expect_identical(graphics::par("mar"), mar0)
  try(env$with_par(list(mar = c(1, 1, 1, 1)), stop("boom")), silent = TRUE)
  expect_identical(graphics::par("mar"), mar0)

  # zip_dir(): working directory restored, also on error
  wd <- getwd()
  expect_error(env$zip_dir(tempfile(fileext = ".zip"), tempfile("does_not_exist_")))
  expect_identical(getwd(), wd)
  if (nzchar(Sys.which("zip"))) {
    src <- tempfile("zipdir_"); dir.create(file.path(src, "masks"), recursive = TRUE)
    writeLines("x", file.path(src, "masks", "m.txt"))
    z <- tempfile(fileext = ".zip")
    expect_true(env$zip_dir(z, src))
    expect_identical(getwd(), wd)
    expect_true("masks/m.txt" %in% utils::unzip(z, list = TRUE)$Name)
    unlink(c(z, src), recursive = TRUE)
  }

  # register_session_cleanup(): removes the folder when the session ends
  callbacks <- list()
  fake_session <- list(onSessionEnded = function(fun) callbacks[[length(callbacks) + 1]] <<- fun)
  tmp <- tempfile("sess_"); dir.create(tmp)
  env$register_session_cleanup(tmp, session = fake_session)
  expect_true(dir.exists(tmp))
  for (cb in callbacks) cb()
  expect_false(dir.exists(tmp))
})

test_that("Shiny app object can be built without changing global options", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  app <- system.file("shiny", "app", package = "LeafInjuryR")
  opts <- options()
  wd <- getwd()
  attached <- "package:shiny" %in% search()
  obj <- suppressPackageStartupMessages(shiny::shinyAppDir(app))
  if (!attached && "package:shiny" %in% search()) detach("package:shiny")
  expect_s3_class(obj, "shiny.appobj")
  expect_identical(getwd(), wd)
  expect_identical(options()[names(opts)], opts)
})
