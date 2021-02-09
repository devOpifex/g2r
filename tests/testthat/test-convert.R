test_that("Convert", {
  obj <- loess(speed ~ dist, data = cars, span = .3)
  obj_df <- to_g2r(obj)
  expect_s3_class(obj_df, "data.frame")

  obj <- lm(speed ~ dist, data = cars)
  obj_df <- to_g2r(obj)
  expect_s3_class(obj_df, "data.frame")

  obj_df <- to_g2r(AirPassengers)
  expect_s3_class(obj_df, "data.frame")

  obj <- cbind(ldeaths, fdeaths, mdeaths)
  obj_df <- to_g2r(obj)
  expect_s3_class(obj_df, "data.frame")

  obj <- acf(lh, plot = FALSE)
  expect_s3_class(to_g2r(obj), "data.frame")

  obj <- cor(mtcars)
  expect_s3_class(to_g2r(obj), "data.frame")

  obj <- stl(mdeaths, "per")
  expect_s3_class(to_g2r(obj), "data.frame")

})
