test_that("State", {
  g <- g2(cars, asp(x, y)) %>%
    fig_interval(selected(fill = "orange"))

  expect_length(g$x$views[[1]]$states, 1)

  g <- g2(cars, asp(x, y)) %>%
    fig_interval(active(stroke = "black"), selected(fill = "orange"))

  expect_length(g$x$views[[1]]$states, 2)

  expect_snapshot(active())
})
