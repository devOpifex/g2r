test_that("Alias", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point() %>%
    aka(dist, "SO FAR")

  expect_equal(g$x$scale$dist$alias, "SO FAR")
})
