test_that("Animation", {
  anim <- Animation$
    new()$
    appear(duration = 2000, delay = 500)

  expect_length(anim$retrieve(), 1)

  g <- g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
    fig_point(anim)

  expect_length(g$x$views[[1]]$animation, 1)

  expect_snapshot(anim)
})
