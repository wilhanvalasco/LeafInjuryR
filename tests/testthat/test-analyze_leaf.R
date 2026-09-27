test_that("analyze_leaf returns a complete, auditable object", {
  syn <- make_synthetic_leaf()
  res <- analyze_leaf(syn$image, multiclass = TRUE)
  expect_s3_class(res, "leaf_analysis")
  for (nm in c("original", "leaf_mask", "healthy_mask", "injury_mask", "overlay",
               "metrics", "parameters", "quality"))
    expect_false(is.null(res[[nm]]), info = nm)
  expect_true("normalized" %in% names(res))
  expect_percent_invariants(res$metrics, multiclass = TRUE)
  expect_true(res$quality$quality_flag %in% c("ok", "warning", "failed"))
  expect_equal(res$metrics$injured_percent, 100 * sum(syn$injury_mask) / sum(syn$leaf_mask),
               tolerance = 0.05)
})

test_that("analyze_leaf works on real example images with every option", {
  f <- leaf_example_images()[2]
  res <- analyze_leaf(f, crop = "auto", normalize = TRUE, multiclass = TRUE)
  expect_percent_invariants(res$metrics, multiclass = TRUE)
  expect_false(is.null(res$normalized))
  expect_true(res$crop$found)
  expect_equal(dim(res$overlay)[1:2], dim(res$leaf_mask))
  expect_equal(res$metrics$file, "leaf_example_02.jpg")
})

test_that("results are reproducible and parameters are stored", {
  f <- leaf_example_images()[1]
  a <- analyze_leaf(f, method = "kmeans", keep_images = FALSE)
  b <- analyze_leaf(f, method = "kmeans", keep_images = FALSE)
  expect_identical(a$leaf_mask, b$leaf_mask)
  p <- get_analysis_parameters(a)
  for (nm in c("package_version", "R_version", "analysis_timestamp", "background_method",
               "tissue_method", "normalization", "crop_method", "thresholds",
               "morphological_parameters", "quality_control_parameters", "seed"))
    expect_false(is.null(p[[nm]]), info = nm)
  flat <- get_analysis_parameters(a, flatten = TRUE)
  expect_true(all(c("parameter", "value") %in% names(flat)))
  expect_s3_class(analysis_report(a), "leaf_report")
})

test_that("empty images are flagged as failed instead of erroring", {
  res <- analyze_leaf(array(0.9, c(40, 30, 3)))
  expect_equal(res$quality$quality_flag, "failed")
  expect_true(any(grepl("empty|No leaf", res$quality$quality_messages)))
})

test_that("warnings do not change results", {
  syn <- make_synthetic_leaf(necrotic_radius = 0)
  res <- analyze_leaf(syn$image)
  expect_equal(res$quality$quality_flag, "warning")   # injury near 0 %
  expect_lt(res$metrics$injured_percent, 0.5)
  direct <- calculate_injury(res$class_map, res$leaf_mask)
  expect_equal(res$metrics$injured_pixels, direct$injured_pixels)
})

test_that("manual crop requires points", {
  expect_error(analyze_leaf(make_synthetic_leaf()$image, crop = "manual"), "crop_points")
})

test_that("config rejects unknown or invalid parameters", {
  expect_error(leaf_config(morphology = list(foo = 1)), "Unknown")
  expect_error(leaf_config(morphology = list(hole_policy = "x")), "hole_policy")
})
