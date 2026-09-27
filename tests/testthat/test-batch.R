test_that("batch continues after failures and writes outputs", {
  syn <- make_synthetic_leaf()
  good <- write_synthetic(syn)
  bad <- tempfile(fileext = ".jpg"); writeLines("broken", bad)
  out <- tempfile("batch_")
  b <- analyze_leaf_batch(c(good, bad), output_dir = out, progress = FALSE)
  expect_s3_class(b, "leaf_batch")
  expect_equal(b$results$status, c("ok", "failed"))
  expect_true(!is.na(b$results$error_message[2]))
  expect_true(file.exists(file.path(out, "results.csv")))
  expect_true(file.exists(file.path(out, "parameters.csv")))
  expect_length(list.files(file.path(out, "masks")), 3)
  expect_length(list.files(file.path(out, "overlays")), 1)
  csv <- utils::read.csv(file.path(out, "results.csv"))
  expect_true(all(c("file", "leaf_pixels", "healthy_pixels", "injured_pixels", "healthy_percent",
                    "injured_percent", "quality_flag", "method") %in% names(csv)))
})

test_that("batch accepts a directory and a callback", {
  d <- tempfile("dir_"); dir.create(d)
  write_image_png(make_synthetic_leaf()$image, file.path(d, "a.png"))
  write_image_png(make_synthetic_leaf(necrotic_radius = 12)$image, file.path(d, "b.png"))
  calls <- 0
  b <- analyze_leaf_batch(d, progress = FALSE, multiclass = TRUE,
                          callback = function(i, n, f, s) calls <<- calls + 1)
  expect_equal(calls, 2)
  expect_true(all(c("chlorotic_percent", "necrotic_percent", "other_percent") %in% names(b$results)))
})
