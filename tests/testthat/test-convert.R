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

  obj <- USAccDeaths %>%
    stl(s.window='periodic') %>%
    forecast::forecast() %>% 
    to_g2r()

  expect_s3_class(obj, "data.frame")

  data(veteran, package = "survival")

  obj <- survival::survfit(survival::Surv(time, status) ~ trt, data = veteran) %>% 
    to_g2r()

  expect_s3_class(obj, "data.frame")

  obj <- acf(lh, plot = FALSE)
  expect_s3_class(to_g2r(obj), "data.frame")

  obj <- cor(mtcars)
  expect_s3_class(to_g2r(obj), "data.frame")

  data(hpc_cv, package = "yardstick")

  hpc_cv %>%
    yardstick::roc_curve(obs, VF:L) %>% 
    to_g2r() %>% 
    expect_s3_class("data.frame")

  
})
