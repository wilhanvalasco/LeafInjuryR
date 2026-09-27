# Small synthetic images are created on the fly; nothing large is stored.
expect_percent_invariants <- function(m, multiclass = FALSE, tol = 1e-8) {
  expect_true(m$injured_percent >= 0 && m$injured_percent <= 100)
  expect_true(m$healthy_percent >= 0 && m$healthy_percent <= 100)
  expect_equal(m$healthy_pixels + m$injured_pixels, m$leaf_pixels)
  expect_equal(m$healthy_percent + m$injured_percent, 100, tolerance = tol)
  if (multiclass) {
    expect_equal(m$chlorotic_pixels + m$necrotic_pixels + m$other_pixels, m$injured_pixels)
  }
}
write_synthetic <- function(syn, ext = "png") {
  f <- tempfile(fileext = paste0(".", ext))
  write_image_png(syn$image, f)
  f
}
