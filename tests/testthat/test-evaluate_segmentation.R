test_that("metrics are correct on a hand-computed example", {
  ref <- matrix(FALSE, 10, 10); ref[1:5, 1:4] <- TRUE        # 20 px
  pred <- matrix(FALSE, 10, 10); pred[1:5, 3:6] <- TRUE      # 20 px, overlap 10
  ev <- evaluate_segmentation(pred, ref)
  expect_equal(c(ev$tp, ev$fp, ev$fn, ev$tn), c(10, 10, 10, 70))
  expect_equal(ev$iou, 10 / 30)
  expect_equal(ev$dice, 0.5)
  expect_equal(ev$accuracy, 0.8)
  expect_equal(ev$sensitivity, 0.5)
  expect_equal(ev$specificity, 70 / 80)
  expect_equal(ev$precision, 0.5)
  expect_equal(ev$f1, ev$dice)
})

test_that("perfect and empty cases", {
  m <- matrix(c(TRUE, FALSE), 4, 4)
  ev <- evaluate_segmentation(m, m)
  expect_equal(ev$iou, 1); expect_equal(ev$specificity, 1)
  e <- evaluate_segmentation(matrix(FALSE, 3, 3), matrix(FALSE, 3, 3))
  expect_true(is.na(e$iou)); expect_true(is.na(e$sensitivity))
})

test_that("region restriction and size checks", {
  ref <- matrix(FALSE, 4, 4); ref[1, ] <- TRUE
  region <- matrix(FALSE, 4, 4); region[1:2, ] <- TRUE
  expect_equal(evaluate_segmentation(ref, ref, region = region)$n_pixels, 8)
  expect_error(evaluate_segmentation(matrix(TRUE, 3, 3), ref), "do not match")
})

test_that("evaluate_leaf_analysis and ground-truth workflow", {
  syn <- make_synthetic_leaf()
  d <- tempfile("gt_"); dir.create(d)
  write_image_png(syn$image, file.path(d, "s.png"))
  write_mask_png(syn$leaf_mask, file.path(d, "s_leaf.png"))
  write_mask_png(syn$injury_mask, file.path(d, "s_inj.png"))
  expect_identical(read_reference_mask(file.path(d, "s_leaf.png")), syn$leaf_mask)
  res <- analyze_leaf(file.path(d, "s.png"), crop = "auto")
  ev <- evaluate_leaf_analysis(res, file.path(d, "s_leaf.png"), file.path(d, "s_inj.png"))
  expect_equal(ev$label, c("leaf", "injury"))
  expect_gt(ev$iou[1], 0.95)
  write.csv(data.frame(image = "s.png", reference_leaf_mask = "s_leaf.png",
                       reference_injury_mask = "s_inj.png"), file.path(d, "gt.csv"), row.names = FALSE)
  man <- read_ground_truth(file.path(d, "gt.csv"))
  v <- validate_ground_truth(man)
  expect_equal(nrow(v), 2)
  expect_error(read_reference_mask(file.path(d, "s_leaf.png"), target_dim = c(5, 5)), "differs")
})
