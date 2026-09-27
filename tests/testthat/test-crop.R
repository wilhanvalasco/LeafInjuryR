test_that("auto_crop_leaf crops around the leaf and records bbox", {
  syn <- make_synthetic_leaf(200, 150)
  cr <- auto_crop_leaf(syn$image)
  info <- attr(cr, "crop_info")
  expect_true(info$found)
  expect_lt(dim(cr)[1], 200)
  full <- uncrop_mask(matrix(TRUE, dim(cr)[1], dim(cr)[2]), info)
  expect_true(all(full[syn$leaf_mask]))  # whole leaf inside crop
})

test_that("crop_leaf_manual uses the bounding box and clips coordinates", {
  img <- read_leaf(leaf_example_images()[1])
  cr <- crop_leaf_manual(img, x = c(100, 300), y = c(50, 200))
  expect_equal(dim(cr)[1:2], c(201, 151))
  cr2 <- crop_leaf_manual(img, x = c(-10, 5000), y = c(-1, 5000))
  expect_equal(dim(cr2)[1:2], dim(img)[1:2])
  expect_error(crop_leaf_manual(img, x = 1, y = 1), "at least two")
})
