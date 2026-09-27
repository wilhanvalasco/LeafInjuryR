test_that("classification only uses pixels inside the leaf mask", {
  syn <- make_synthetic_leaf()
  arr <- EBImage::imageData(syn$image)
  arr[1:10, 1:10, ] <- c(0.4, 0.25, 0.1)  # brown object on the background
  tis <- classify_leaf_tissue(arr, syn$leaf_mask)
  expect_true(all(tis$class_map[!syn$leaf_mask] == 0L))
  expect_false(any(tis$injury_mask[!syn$leaf_mask]))
})

test_that("known necrotic area is quantified correctly", {
  syn <- make_synthetic_leaf()
  tis <- classify_leaf_tissue(syn$image, syn$leaf_mask, "lab")
  m <- calculate_injury(tis$class_map, syn$leaf_mask)
  expect_equal(m$leaf_pixels, sum(syn$leaf_mask))
  expect_equal(m$injured_pixels, sum(syn$injury_mask))
  expect_percent_invariants(m)
})

test_that("multiclass is consistent with binary", {
  syn <- make_synthetic_leaf(chlorotic = TRUE)
  for (meth in leaf_methods()$tissue_method) {
    b <- classify_leaf_tissue(syn$image, syn$leaf_mask, meth, multiclass = FALSE)
    mc <- classify_leaf_tissue(syn$image, syn$leaf_mask, meth, multiclass = TRUE)
    mb <- calculate_injury(b); mm <- calculate_injury(mc)
    expect_equal(mb$injured_pixels, mm$injured_pixels, info = meth)
    expect_percent_invariants(mm, multiclass = TRUE)
  }
  mc <- classify_leaf_tissue(syn$image, syn$leaf_mask, "lab", multiclass = TRUE)
  expect_gt(mean(mc$class_map[syn$necrotic_mask] == 3L), 0.95)
  expect_gt(mean(mc$class_map[syn$chlorotic_mask] == 2L), 0.95)
})

test_that("calculate_injury ignores class codes outside the leaf mask", {
  cm <- matrix(2L, 10, 10); leaf <- matrix(FALSE, 10, 10); leaf[1:5, ] <- TRUE
  cm[1:5, ] <- 1L; cm[1, 1] <- 2L
  m <- calculate_injury(cm, leaf)
  expect_equal(m$leaf_pixels, 50); expect_equal(m$injured_pixels, 1)
  expect_equal(m$injured_percent, 2)
})

test_that("empty leaf mask gives NA percentages, not division errors", {
  m <- calculate_injury(matrix(0L, 5, 5), matrix(FALSE, 5, 5))
  expect_equal(m$leaf_pixels, 0); expect_true(is.na(m$injured_percent))
})
