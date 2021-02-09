test_that("Info line", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point() %>%
    info_line(
      asp(
        x = 4,
        y = 2,
        xend = 25,
        yend = 120,
        content = "A diagonal line"
      )
    )

  expect_length(g$x$annotations, 1)
})

test_that("Info data marker", {
  samp <- cars[sample(nrow(cars), 10), ]
  samp$cnt <- letters[1:10]

  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_marker(
      asp(speed, dist, content = cnt), 
      top = TRUE,
      data = samp
    )

  expect_length(g$x$annotations, 10)
  expect_equal(g$x$annotations[[1]]$type, "dataMarker")
})

test_that("Info h & v line", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_hline(asp(y = 43, content = "Mean Distance")) %>% 
    info_vline(asp(x = 15, content = "Mean Speed"))

  expect_length(g$x$annotations, 2)
})

test_that("Info region filter", {
  df <- data.frame(
    x = 1:100,
    y = runif(100, -5, 5)
  )

  g <- g2(df, asp(x, y, color = "white", shape = "smooth")) %>% 
    fig_area() %>% 
    fig_line() %>% 
    info_region_filter(
      asp(
        x = "min", y = 0, 
        xend = "max", yend = "min"
      ), 
      top = TRUE,
      color = "red"
    ) %>% 
    info_region_filter(
      asp(
        x = "min", y = "max", 
        xend = "max", yend = 0
      ), 
      top = TRUE,
      color = "green"
    )

  expect_length(g$x$annotations, 2)
  expect_equal(g$x$annotations[[1]]$type, "regionFilter")
})

test_that("Info text", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_text(
      asp(x = "median", y = "median"),
      content = "Median"
    )

  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "text")
})

test_that("Info region", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_region(
      asp(
        x = 10, y = "max",
        xend = 20, yend = "min"
      )
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "region")
})
