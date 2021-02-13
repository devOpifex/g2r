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

test_that("Info image", {
  uri <- "https://raw.githubusercontent.com/devOpifex/media/master/opifex-banner.png"
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_image(
      src = uri
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "image")
  expect_equal(g$x$annotations[[1]]$opts$src, uri)
})

test_that("Info arc", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_arc(
      asp(
        start = 5, end = 20
      )
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "arc")
  expect_equal(g$x$annotations[[1]]$opts$start, 5)
})

test_that("Info abline", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_abline(
      asp(
        x = 0, y = 0, 
        xend = "max", yend = "max",
        content = "A diagonal line"
      )
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "line")
  expect_equal(g$x$annotations[[1]]$opts$position, list("min", "min"))

  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() %>% 
    info_abline(
      asp(
        x = 0, y = 0, 
        xend = "max", yend = "max",
        content = "A diagonal line"
      ),
      direction = 2
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "line")
  expect_equal(g$x$annotations[[1]]$opts$position, list("min", "max"))
})

test_that("Info data region", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_line() %>% 
    info_data_region(
      asp(
        x = 10, y = 18,
        xend = 13, yend = 26
      )
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "dataRegion")
  expect_equal(g$x$annotations[[1]]$opts$start, list(10, 18))
})

test_that("Info shape", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_line() %>% 
    info_shape(
      asp(
        x = 10, y = 18,
        xend = 13, yend = 26
      )
    )
  
  expect_length(g$x$annotations, 1)
  expect_equal(g$x$annotations[[1]]$type, "shape")
})
