test_that("Info", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_line(
      asp(
        x = 4, y = 2, 
        xend = 25, yend = 120,
        content = "A diagonal line"
      )
    )

  expect_length(g$x$annotations, 1)
})
