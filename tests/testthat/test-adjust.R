test_that("Adjust", {
  expect_error(adjust())

  g <- g2(cars, asp(dist, speed)) %>%
    fig_interval(adjust("stack"))

  expect_length(g$x$views[[1]]$adjust, 1)

  g <- g2(cars, asp(dist, speed)) %>%
    fig_interval(adjust("stack"), adjust("symmetric"))

  expect_length(g$x$views[[1]]$adjust, 2)

  # print
  expect_snapshot(adjust("stack"))

  g <- g2(cars, asp(dist, speed)) %>%
    fig_interval(adjust("stack", .5, speed))

  expect_equal(
    g$x$views[[1]]$adjust,
    list(structure(list(type = "stack", marginRatio = 0.5, dodgeBy = "speed"), class = c("adjust", "list")))
  )
})
