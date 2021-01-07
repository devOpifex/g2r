test_that("Scale", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point()

  # time
  time <- g %>% 
    gauge_x_time(range = c(0, 1)) %>% 
    gauge_y_time(range = c(0, 1))
  expect_equal(time$x$scale[["speed"]]$type, "time")
  expect_equal(time$x$scale[["dist"]]$type, "time")

  # linear
  linear <- g %>% 
    gauge_x_linear(range = c(0, 1)) %>% 
    gauge_y_linear(range = c(0, 1))
  expect_equal(linear$x$scale[["speed"]]$type, "linear")
  expect_equal(linear$x$scale[["dist"]]$type, "linear")

  # cat
  cat <- g %>% 
    gauge_x_cat(range = c(0, 1)) %>% 
    gauge_y_cat(range = c(0, 1))
  expect_equal(cat$x$scale[["speed"]]$type, "cat")
  expect_equal(cat$x$scale[["dist"]]$type, "cat")

  # cat
  time_cat <- g %>% 
    gauge_x_time_cat(range = c(0, 1)) %>% 
    gauge_y_time_cat(range = c(0, 1))
  expect_equal(time_cat$x$scale[["speed"]]$type, "timeCat")
  expect_equal(time_cat$x$scale[["dist"]]$type, "timeCat")

  # log
  log <- g %>% 
    gauge_x_log(range = c(0, 1)) %>% 
    gauge_y_log(range = c(0, 1))
  expect_equal(log$x$scale[["speed"]]$type, "log")
  expect_equal(log$x$scale[["dist"]]$type, "log")

  # pow
  pow <- g %>% 
    gauge_x_pow(range = c(0, 1)) %>% 
    gauge_y_pow(range = c(0, 1))
  expect_equal(pow$x$scale[["speed"]]$type, "pow")
  expect_equal(pow$x$scale[["dist"]]$type, "pow")

  # quantile
  quantile <- g %>% 
    gauge_x_quantile(range = c(0, 1)) %>% 
    gauge_y_quantile(range = c(0, 1))
  expect_equal(quantile$x$scale[["speed"]]$type, "quantile")
  expect_equal(quantile$x$scale[["dist"]]$type, "quantile")

  # quantize
  quantize <- g %>% 
    gauge_x_quantize(range = c(0, 1)) %>% 
    gauge_y_quantize(range = c(0, 1))
  expect_equal(quantize$x$scale[["speed"]]$type, "quantize")
  expect_equal(quantize$x$scale[["dist"]]$type, "quantize")

  # identity
  identity <- g %>% 
    gauge_x_identity(range = c(0, 1)) %>% 
    gauge_y_identity(range = c(0, 1))
  expect_equal(identity$x$scale[["speed"]]$type, "identity")
  expect_equal(identity$x$scale[["dist"]]$type, "identity")

  # asps
  asps <- g %>% 
    gauge_asp(speed, type = "identity", range = c(0, 1)) %>% 
    gauge_asp(dist, type = "identity", range = c(0, 1))
  expect_equal(asps$x$scale[["speed"]]$type, "identity")
  expect_equal(asps$x$scale[["dist"]]$type, "identity")
})
