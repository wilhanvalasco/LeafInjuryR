test_that("read_leaf reads example images with metadata", {
  img <- read_leaf(leaf_example_images()[1])
  expect_s4_class(img, "Image")
  expect_equal(dim(img)[3], 3)
  md <- leaf_image_metadata(img)
  expect_equal(md$extension, "jpg")
  expect_equal(c(md$width, md$height), dim(img)[1:2])
})

test_that("read_leaf gives clear errors", {
  expect_error(read_leaf("does_not_exist.jpg"), "could not be read", class = "leafinjury_error")
  f <- tempfile(fileext = ".bmp"); writeLines("x", f)
  expect_error(read_leaf(f), "Unsupported image format", class = "leafinjury_unsupported_format")
  g <- tempfile(fileext = ".png"); writeLines("not an image", g)
  expect_error(read_leaf(g), "could not be read")
})

test_that("grayscale images are rejected as incompatible", {
  f <- tempfile(fileext = ".png")
  EBImage::writeImage(EBImage::Image(matrix(0.5, 20, 20)), f)
  expect_error(read_leaf(f), "compatible colour channels")
})

test_that("RGBA images drop alpha", {
  f <- tempfile(fileext = ".png")
  EBImage::writeImage(EBImage::Image(array(0.5, c(10, 10, 4)), colormode = "Color"), f)
  expect_equal(dim(read_leaf(f))[3], 3)
})
