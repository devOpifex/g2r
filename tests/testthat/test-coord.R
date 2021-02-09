test_that("Coordinates", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_point() %>%
    coord_scale(-1, 1) %>% 
    coord_type("polar") %>%
    coord_rotate() %>%
    coord_reflect() %>%
    coord_transpose() 

  expect_equal(g$x$coord$type, "polar")
  expect_equal(g$x$coordRotate, 90)
  expect_equal(g$x$coordReflect, "xy")
  expect_equal(g$x$coordTranspose, TRUE)
  expect_equal(g$x$coordScale, list(-1, 1))
})
