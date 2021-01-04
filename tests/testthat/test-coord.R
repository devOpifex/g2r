test_that("Coordinates", {
  g <- g2(cars, asp(dist, speed)) %>% 
    fig_point() %>% 
    coord_type("polar")

  expect_equal(g$x$coord$type, "polar")
})
