test_that("auto segmentation recovers the synthetic leaf, including necrosis", {
  syn <- make_synthetic_leaf()
  seg <- segment_leaf_mask(syn$image, "auto")
  ev <- evaluate_segmentation(seg$mask, syn$leaf_mask)
  expect_gt(ev$iou, 0.97)
  # necrotic pixels must belong to the leaf mask
  expect_gt(mean(seg$mask[syn$necrotic_mask]), 0.99)
})

test_that("all methods return a logical mask of the right size", {
  syn <- make_synthetic_leaf()
  for (m in leaf_methods()$method) {
    seg <- segment_leaf_mask(syn$image, m)
    expect_true(is.logical(seg$mask), info = m)
    expect_equal(dim(seg$mask), dim(syn$leaf_mask), info = m)
    expect_gt(evaluate_segmentation(seg$mask, syn$leaf_mask)$iou, 0.85)
  }
})

test_that("dark backgrounds are handled through border polarity", {
  syn <- make_synthetic_leaf()
  arr <- EBImage::imageData(syn$image)
  for (ch in 1:3) { l <- arr[, , ch]; l[!syn$leaf_mask] <- 0.05; arr[, , ch] <- l }
  seg <- segment_leaf_mask(arr, "lab")
  expect_gt(evaluate_segmentation(seg$mask, syn$leaf_mask)$iou, 0.9)
})

test_that("threshold argument is validated", {
  syn <- make_synthetic_leaf()
  expect_error(segment_leaf_mask(syn$image, "auto", threshold = 10), "not applicable")
  seg <- segment_leaf_mask(syn$image, "lab", threshold = 20)
  expect_equal(seg$thresholds$chroma, 20)
})

test_that("components are recorded, not silently removed", {
  syn <- make_synthetic_leaf()
  arr <- EBImage::imageData(syn$image)
  arr[5:12, 5:12, 1] <- 0.3; arr[5:12, 5:12, 2] <- 0.6; arr[5:12, 5:12, 3] <- 0.1
  seg <- segment_leaf_mask(arr, "auto")
  expect_true(any(!seg$components$kept))
  expect_true(all(seg$components$reason[!seg$components$kept] %in%
                    c("below_min_object_size", "not_largest_component")))
  expect_false(any(seg$mask[5:12, 5:12]))
})

test_that("background-coloured holes are perforation candidates, not leaf", {
  syn <- make_synthetic_leaf(necrotic_radius = 0)
  arr <- EBImage::imageData(syn$image)
  hole <- matrix(FALSE, dim(arr)[1], dim(arr)[2]); hole[70:77, 40:47] <- TRUE
  for (ch in 1:3) { l <- arr[, , ch]; l[hole] <- syn$colors$background[ch]; arr[, , ch] <- l }
  seg <- segment_leaf_mask(arr, "auto")
  expect_false(any(seg$mask[hole]))
  expect_gt(seg$holes$perforation_candidate_pixels, 0)
  seg2 <- segment_leaf_mask(arr, "auto", config = leaf_config(morphology = list(hole_policy = "fill_all")))
  expect_true(all(seg2$mask[hole]))
})

test_that("empty images yield empty masks without error", {
  arr <- array(0.9, c(40, 30, 3))
  seg <- segment_leaf_mask(arr, "auto")
  expect_false(any(seg$mask))
})
