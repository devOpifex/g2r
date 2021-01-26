test_that("Legend", {
  g <- g2(cars, asp(dist, speed, color = speed)) %>%
    fig_point(asp(size = dist))

  l <- g %>%
    legend_color(position = "top") %>%
    legend_size(FALSE)

  expect_length(l$x$legend, 2)
  expect_false(l$x$legend[[2]]$opts)
  expect_equal(l$x$legend[[1]]$opts, list(position = "top"))

  l2 <- legend_asps(g, "dist", FALSE)
  expect_false(l2$x$legend[[1]]$opts)
})
