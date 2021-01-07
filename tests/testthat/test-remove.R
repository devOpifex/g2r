test_that("Remove", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    fig_line(config("LINE"))
  
  expect_length(g$x$views, 2)

  g <- remove_figure(g, 1)

  expect_length(g$x$views, 1)

  g <- remove_figure(g, "LINE")
  expect_length(g$x$views, 0)
})
