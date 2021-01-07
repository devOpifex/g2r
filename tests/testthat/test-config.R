test_that("Config", {
  g <- g2(cars, asp(dist, speed)) %>% 
    fig_point(config("POINT"))

  expect_equal(g$x$views[[1]]$conf$id, "POINT")

  expect_snapshot(config())
})
