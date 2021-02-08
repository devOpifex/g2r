test_that("Options", {
  expect_error(g2() %>% slider())
  expect_error(g2() %>% scrollbar())

  g <- g2(cars, asp(speed, dist)) %>%
    slider(FALSE) %>%
    scrollbar(FALSE)

  expect_false(g$x$slider)
  expect_false(g$x$scrollbar)
})
