ex <- function(k) system.file("extdata", sprintf("leaf_example_%02d.jpg", k), package = "LeafInjuryR")

test_that("the package exports exactly the six public functions", {
  expect_setequal(getNamespaceExports("LeafInjuryR"),
                  c("analyze_leaf", "analyze_leaf_batch", "crop_leaf", "segment_leaf",
                    "validate_leaf", "run_leafinjury_app"))
})

test_that("crop_leaf preserves resolution and pixel values", {
  img <- read_leaf(ex(1))
  man <- crop_leaf(img, "manual", x = c(101, 300), y = c(51, 200))
  expect_equal(dim(man)[1:2], c(200, 150))
  expect_identical(EBImage::imageData(man), EBImage::imageData(img)[101:300, 51:200, , drop = FALSE])
  auto <- crop_leaf(ex(1))
  info <- attr(auto, "crop_info")
  expect_true(info$found)
  bb <- info$bbox
  expect_identical(EBImage::imageData(auto),
                   EBImage::imageData(img)[bb[["x_min"]]:bb[["x_max"]], bb[["y_min"]]:bb[["y_max"]], , drop = FALSE])
  expect_error(crop_leaf(img, "manual"), "coordinates")
})

test_that("segment_leaf auto: leaf mask, classes and metrics are consistent", {
  syn <- make_synthetic_leaf(chlorotic = TRUE)
  s <- segment_leaf(syn$image, multiclass = TRUE)
  expect_s3_class(s, "leaf_segmentation")
  expect_gt(evaluate_segmentation(s$leaf_mask, syn$leaf_mask)$iou, 0.97)
  expect_true(all(s$class_map[!s$leaf_mask] == 0L))
  m <- s$metrics
  expect_equal(m$healthy_pixels + m$injured_pixels, m$leaf_pixels)
  expect_equal(m$chlorotic_pixels + m$necrotic_pixels + m$other_pixels, m$injured_pixels)
})

test_that("segment_leaf manual corrections behave as documented", {
  syn <- make_synthetic_leaf(necrotic_radius = 10)
  s <- segment_leaf(syn$image)
  inj0 <- s$metrics$injured_pixels
  fix_h <- data.frame(action = "healthy", shape = "rect", xmin = 1, xmax = 120, ymin = 1, ymax = 90)
  s1 <- segment_leaf(syn$image, "manual", corrections = fix_h, segmentation = s)
  expect_equal(s1$metrics$injured_pixels, 0)
  expect_equal(s1$metrics$leaf_pixels, s$metrics$leaf_pixels)
  fix_r <- data.frame(action = "remove_leaf", shape = "circle", x = 60, y = 45, radius = 5)
  s2 <- segment_leaf(syn$image, "manual", corrections = fix_r, segmentation = s)
  expect_lt(s2$metrics$leaf_pixels, s$metrics$leaf_pixels)
  expect_false(any(s2$class_map[!s2$leaf_mask] != 0L))
  fix_a <- data.frame(action = "add_leaf", shape = "rect", xmin = 1, xmax = 5, ymin = 1, ymax = 5)
  s3 <- segment_leaf(syn$image, "manual", corrections = fix_a, segmentation = s)
  expect_equal(s3$metrics$leaf_pixels, s$metrics$leaf_pixels + 25)
  # tissue actions never act outside the leaf
  fix_i <- data.frame(action = "injured", shape = "rect", xmin = 1, xmax = 5, ymin = 1, ymax = 5)
  s4 <- segment_leaf(syn$image, "manual", corrections = fix_i, segmentation = s)
  expect_equal(s4$metrics$injured_pixels, inj0)
  # corrections are stored and the result is reproducible
  both <- rbind(as_corrections(fix_h), as_corrections(fix_r))
  a <- segment_leaf(syn$image, "manual", corrections = both)
  b <- segment_leaf(syn$image, "manual", corrections = both)
  expect_identical(a$class_map, b$class_map)
  expect_equal(nrow(a$corrections), 2)
  expect_error(segment_leaf(syn$image, "manual"), "corrections")
  expect_error(segment_leaf(syn$image, "manual",
                            corrections = data.frame(action = "paint", shape = "rect")), "Unknown")
})

test_that("relative tissue method uses the leaf's own reference colour", {
  syn <- make_synthetic_leaf()
  arr <- EBImage::imageData(syn$image)
  # turn the healthy tissue purple-red (pigmented leaf): absolute-green rules fail
  for (ch in 1:3) { l <- arr[, , ch]; l[syn$healthy_mask] <- c(0.45, 0.18, 0.35)[ch]; arr[, , ch] <- l }
  lab <- calculate_injury(classify_leaf_tissue(arr, syn$leaf_mask, "lab"))
  rel <- calculate_injury(classify_leaf_tissue(arr, syn$leaf_mask, "relative"))
  expect_gt(lab$injured_percent, 90)
  expect_lt(rel$injured_percent, 20)
})

test_that("analyze_leaf uses supplied segmentation, corrections and scale", {
  img <- crop_leaf(ex(2))
  s <- segment_leaf(img)
  fix <- data.frame(action = "healthy", shape = "rect", xmin = 1, xmax = 200, ymin = 1, ymax = 200)
  s2 <- segment_leaf(img, "manual", corrections = fix, segmentation = s)
  r <- analyze_leaf(img, segmentation = s2, pixels_per_cm = 40, keep_images = FALSE)
  expect_equal(r$metrics$injured_percent, s2$metrics$injured_percent)
  expect_equal(r$metrics$manual_corrections, 1)
  expect_equal(r$metrics$leaf_area_cm2, r$metrics$leaf_pixels / 1600)
  expect_equal(nrow(r$parameters$manual_corrections), 1)
  expect_error(analyze_leaf(img, pixels_per_cm = -1), "positive")
  r0 <- analyze_leaf(img, keep_images = FALSE)
  expect_null(r0$metrics$leaf_area_cm2)
  expect_equal(r0$parameters$area_unit, "pixels")
  expect_true(!is.na(analyze_leaf(ex(1), keep_images = FALSE)$metrics$file_md5))
})

test_that("validate_leaf reports accuracy only with reference masks", {
  res <- analyze_leaf(ex(1), crop = "auto")
  v0 <- validate_leaf(res)
  expect_s3_class(v0, "leaf_validation")
  expect_null(v0$metrics)
  expect_false(v0$has_reference)
  expect_true(v0$quality$quality_flag %in% c("ok", "warning", "failed"))
  full <- uncrop_mask(res$leaf_mask, res$crop)
  inj <- uncrop_mask(res$injury_mask, res$crop)
  v1 <- validate_leaf(res, full, inj)
  expect_equal(v1$metrics$iou, c(1, 1))
  expect_equal(v1$metrics$injured_percent_error[2], 0, tolerance = 1e-8)
  f <- tempfile(fileext = ".png"); grDevices::png(f); plot(v1); grDevices::dev.off()
  expect_true(file.exists(f))
  expect_error(validate_leaf(res, matrix(TRUE, 5, 5)), "match")
})

test_that("batch reports: HTML, Excel and CSV share the same results", {
  syn <- make_synthetic_leaf()
  good <- tempfile(fileext = ".png"); write_image_png(syn$image, good)
  bad <- tempfile(fileext = ".jpg"); writeLines("broken", bad)
  out <- tempfile("rep_")
  b <- analyze_leaf_batch(c(good, bad), output_dir = out, progress = FALSE)
  expect_setequal(names(b$report_files), c("html", "xlsx", "csv"))
  csv <- utils::read.csv(file.path(out, "results.csv"))
  expect_equal(csv$injured_percent[1], b$results$injured_percent[1])
  expect_equal(csv$status, c("ok", "failed"))
  expect_identical(readBin(file.path(out, "results.xlsx"), "raw", 2), charToRaw("PK"))
  html <- paste(readLines(file.path(out, "report.html"), warn = FALSE), collapse = "\n")
  expect_match(html, basename(good), fixed = TRUE)
  expect_match(html, "Traceability", fixed = TRUE)
  expect_match(html, "data:image/jpeg;base64", fixed = TRUE)
  expect_false(grepl("<script src=|<link [^>]*href=.http", html))   # self-contained
  expect_equal(b$summary$value[b$summary$indicator == "Analyses with error"], 1)
  v <- validate_leaf(b)
  expect_null(v$metrics)
  expect_error(analyze_leaf_batch(good, corrections = data.frame()), "not used in batch")
})

test_that("single analysis can produce the same report formats", {
  res <- analyze_leaf(make_synthetic_leaf()$image)
  d <- tempfile("one_")
  files <- write_leaf_reports(res, d)
  expect_true(all(file.exists(files)))
})

test_that("Shiny app files exist, are ASCII and parse", {
  app <- system.file("shiny", "app", package = "LeafInjuryR")
  files <- c(file.path(app, "app.R"), list.files(file.path(app, "modules"), full.names = TRUE))
  for (f in files) {
    expect_silent(parse(f))
    expect_false(any(grepl("[^\\x01-\\x7F]", readLines(f, warn = FALSE), perl = TRUE)), info = f)
  }
  expect_true(file.exists(file.path(app, "www", "leafinjury.css")))
})
