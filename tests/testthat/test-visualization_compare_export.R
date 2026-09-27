test_that("overlay keeps background pixels and does not modify input", {
  syn <- make_synthetic_leaf()
  before <- EBImage::imageData(syn$image)
  tis <- classify_leaf_tissue(syn$image, syn$leaf_mask)
  ov <- EBImage::imageData(create_leaf_overlay(syn$image, tis$class_map, alpha = 0.5, outline = FALSE))
  expect_identical(EBImage::imageData(syn$image), before)
  bg <- !syn$leaf_mask
  expect_equal(ov[, , 1][bg], before[, , 1][bg])
  expect_false(isTRUE(all.equal(ov[, , 2][syn$leaf_mask], before[, , 2][syn$leaf_mask])))
  expect_error(create_leaf_overlay(syn$image, tis$class_map, alpha = 2), "alpha")
})

test_that("compare_segmentation_methods returns masks and agreement", {
  syn <- make_synthetic_leaf()
  cmp <- compare_segmentation_methods(syn$image)
  expect_setequal(names(cmp$masks), c("hsv", "lab", "exg", "otsu", "auto"))
  expect_equal(dim(cmp$agreement), c(5, 5))
  expect_true(all(diag(cmp$agreement) == 1))
  f <- tempfile(fileext = ".png"); grDevices::png(f); plot(cmp); grDevices::dev.off()
  expect_true(file.exists(f))
  tc <- compare_tissue_methods(syn$image, syn$leaf_mask)
  expect_equal(nrow(tc$summary), 5)
})

test_that("export writes CSV and PNG files", {
  res <- analyze_leaf(make_synthetic_leaf()$image, normalize = TRUE)
  files <- export_leaf_analysis(res, tempfile("exp_"), prefix = "x")
  expect_true(all(file.exists(files)))
  expect_true(any(grepl("_overlay.png$", files)))
  expect_true(any(grepl("_normalized.png$", files)))
  f <- tempfile(fileext = ".png"); grDevices::png(f); plot(res); grDevices::dev.off()
  expect_true(file.exists(f))
})

test_that("normalisation never modifies the original image", {
  img <- read_leaf(leaf_example_images()[1])
  before <- EBImage::imageData(img)
  n <- normalize_leaf_image(img, flatten_illumination = TRUE)
  expect_identical(EBImage::imageData(img), before)
  g <- attr(n, "normalization")$white_balance_gains
  expect_true(all(g >= 1 / 1.3 & g <= 1.3))
})
