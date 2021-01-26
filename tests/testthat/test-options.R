test_that("Options", {
  g <- g2(cars, asp(speed, dist)) %>%
    slider(FALSE) %>%
    scrollbar(FALSE)

  expect_false(g$x$slider)
  expect_false(g$x$scrollbar)
})
