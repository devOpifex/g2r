test_that("Figures - Point", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point()

  expect_length(g$x$data, 2)
  expect_length(g$x$main_asp, 2)
  expect_length(g$x$views, 1)
  expect_equal(g$x$views[[1]]$type, "point")
  expect_equal(g$x$views[[1]]$position, "speed*dist")

  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(shape = "square"))

  expect_equal(g$x$views[[1]]$shape, "square")

  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(size = dist))

  expect_equal(g$x$views[[1]]$size, "dist")

  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(size = 3))

  expect_equal(g$x$views[[1]]$size, 3)

  expect_length(g$x$cols, 3)

  expect_snapshot(asp(hello, world))
})

test_that("Figures - Line", {
  g <- g2(cars) %>%
    fig_point(asp(dist, speed)) %>%
    fig_line(asp(speed, dist, shape = "smooth"))

  expect_length(g$x$data, 2)
  expect_length(g$x$main_asp, 0)
  expect_length(g$x$views, 2)
  expect_equal(g$x$views[[2]]$type, "line")
  expect_equal(g$x$views[[2]]$position, "speed*dist")
  expect_equal(g$x$views[[2]]$shape, "smooth")
})

test_that("Figures - Area", {
  g <- g2(cars) %>%
    fig_area(asp(dist, speed, shape = "hv"))

  expect_length(g$x$data, 2)
  expect_length(g$x$main_asp, 0)
  expect_length(g$x$views, 1)
  expect_equal(g$x$views[[1]]$type, "area")
  expect_equal(g$x$views[[1]]$position, "dist*speed")
  expect_equal(g$x$views[[1]]$shape, "hv")
})

test_that("Figures - Interval", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_interval(asp(speed, dist))

  expect_equal(g$x$views[[1]]$type, "interval")
  expect_equal(g$x$views[[1]]$position, "speed*dist")

  g <- g2(cars, asp(y = speed)) %>%
    fig_interval(asp(x = dist))

  expect_equal(g$x$views[[1]]$position, "dist*speed")

  g <- g2(cars, asp(y = speed)) %>%
    fig_interval(asp(x = dist), inherit_asp = FALSE)

  expect_equal(g$x$views[[1]]$position, "dist")
})

test_that("Figures - Polygon", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_polygon(fillOpacity = .3)

  expect_equal(g$x$views[[1]]$type, "polygon")
  expect_length(g$x$views[[1]]$style, 1)
  expect_equal(g$x$views[[1]]$style$fillOpacity, .3)
})

test_that("Figures - Edge", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_edge()

  expect_equal(g$x$views[[1]]$type, "edge")
})

test_that("Figures - Schema", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_schema()

  expect_equal(g$x$views[[1]]$type, "schema")
})

test_that("Figures - Path", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_path()

  expect_equal(g$x$views[[1]]$type, "path")
})

test_that("Figures - Heatmap", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_heatmap()

  expect_equal(g$x$views[[1]]$type, "heatmap")
})

test_that("Figures - style", {
  g <- g2(cars, asp(dist, speed)) %>%
    fig_point(fillOpacity = .4)

  expect_equal(g$x$views[[1]]$style, list(fillOpacity = .4))

  g <- g2(cars, asp(dist, speed, style = speed)) %>%
    fig_point(fillOpacity = .4)

  expect_equal(g$x$views[[1]]$style[[1]], "speed")
  expect_equal(g$x$views[[1]]$style[[2]], list(fillOpacity = .4))
})

test_that("Figures - bin", {
  g <- g2(cars, asp(speed, dist, color = count)) %>% 
    fig_bin()

  expect_error(g2(cars) %>% fig_bin())

  expect_equal(g$x$views[[1]]$type, "polygon")
})

test_that("Figures - ribbon & range", {
  expect_error(g2() %>% fig_range())
  expect_error(g2() %>% fig_ribbon())

  df <- data.frame(
    x = 1:50,
    y = rnorm(50, mean = rep(c(67,73),each=25), sd = 2)
  )

  df$ymin <- df$y - runif(50, 2, 3)
  df$ymax <- df$y + runif(50, 2, 4)

  g <- g2(df, asp(x, ymin = ymin, ymax = ymax)) %>%  
    fig_ribbon()

  expect_length(g$x$views, 1)
  expect_equal(g$x$views[[1]]$type, "area")

  g <- g2(df, asp(x, ymin = ymin, ymax = ymax)) %>%  
    fig_range()
  
  expect_equal(g$x$views[[1]]$type, "interval")
})

test_that("Figures - histogram", {
  df <- data.frame(
    grp = rep(c("A", "B"), each = 200),
    val = c(
      rnorm(200, mean = 57, sd = 5), 
      rnorm(200, mean = 53, sd = 5)
    )
  )

  g <- g2(df, asp(val, color = grp)) %>% 
    fig_histogram(bin_width = 1, fillOpacity = .5)

  expect_equal(g$x$views[[1]]$type, "interval")

  g <- g2(df, asp(val)) %>% 
    fig_histogram(bin_width = 1, fillOpacity = .5)

  expect_equal(g$x$views[[1]]$type, "interval")
})

test_that("Figures - boxplot", {
  df <- reshape(
    iris,
    varying = names(iris)[1:4],
    direction = "long",
    v.names = "value",
    idvar = "Species",
    new.row.names = 1:600,
    timevar = "var",
    times = names(iris)[1:4]
  )

  g <- g2(df, asp(name, value, color = Species)) %>% 
    fig_boxplot(adjust("dodge"))

  expect_equal(g$x$views[[1]]$type, "schema")
})

test_that("Figures - smooth", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_smooth(asp(shape = "smooth"), method = "polynomial")

  expect_equal(g$x$views[[1]]$type, "line")

  expect_error(g2() %>% fig_smooth())

  g <- g2(cars, asp(speed, dist)) %>% 
    fig_smooth()

  expect_equal(g$x$views[[1]]$type, "line")

  g <- g2(cars, asp(speed, dist, color = dist)) %>% 
    fig_smooth()

  expect_equal(g$x$views[[1]]$type, "line")

  df <- cars
  df$color <- rep(c("A", "B"), 25)

  g <- g2(df, asp(speed, dist, color = color)) %>% 
    fig_smooth()

  expect_equal(g$x$views[[1]]$type, "line")
})

test_that("Figures - density", {
  expect_error(g2() %>% fig_density())

  g <- g2(iris, asp(Sepal.Length, color = Species)) %>% 
    fig_density()
  
  expect_equal(g$x$views[[1]]$type, "area")

  g <- g2(iris, asp(Sepal.Length)) %>% 
    fig_density()
  
  expect_equal(g$x$views[[1]]$type, "area")
})

test_that("Figures - pie", {
  df <- data.frame(
    name = letters[1:5],
    value = runif(5)
  )

  g <- g2(df, asp(y = value, color = name, label = name)) %>% 
    fig_pie()

  expect_equal(g$x$views[[1]]$type, "interval")
})

test_that("Figures - voronoi", {
  expect_error(g2(cars, asp(speed, dist)) %>% fig_voronoi())

  df <- data.frame(
    x = runif(50, 1, 500),
    y = runif(50, 1, 500),
    value = runif(50, 1, 500),
    stringsAsFactors = FALSE
  )

  g <- g2(df, asp(x, y, color = value)) %>% 
    fig_voronoi()

  expect_equal(g$x$views[[1]]$type, "polygon")
})

test_that("Figures - waffle", {
  expect_error(g2(cars) %>% fig_waffle())
  
  fruits <- data.frame(
    fruit = c("Apples", "Bananas", "Pears", "Oranges"),
    value = c(.45, .15, .35, .05) * 100,
    stringsAsFactors = FALSE
  )

  g <- g2(fruits, asp(value, color = fruit)) %>% 
    fig_waffle()
  
  expect_equal(g$x$views[[1]]$type, "point")
})

test_that("Figures - rug", {
  g <- g2(mtcars, asp(wt, mpg)) %>% 
    fig_point() %>% 
    fig_rug(asp(size = 10)) %>% 
    fig_rug(asp(size = 10), axis = "y")

  expect_equal(g$x$views[[2]]$type, "point")
  expect_equal(g$x$views[[3]]$type, "point")
})

test_that("Figures - candle", {
  expect_error(g2(cars) %>% fig_candle())

  stock <- structure(
    list(
      date = structure(c(18626, 18627, 18631, 18632), class = "Date"),
      open = c(39.52, 39.330002, 40.169998, 41.5),
      high = c(
        39.73,
        40,
        41.560001,
        42.040001
      ),
      low = c(
        39.200001,
        39.029999,
        39.939999,
        40.77
      ),
      close = c(
        39.34,
        39.880001,
        41.400002,
        41.16
      )
    ),
    row.names = c(NA, -4L),
    class = c(
      "tbl_df",
      "tbl",
      "data.frame"
    )
  )
  
  g <- g2(stock, asp(date, open = open, close = close, high = high, low = low)) %>%
    fig_candle()

  expect_equal(g$x$views[[1]]$type, "schema")
})

test_that("Figures - error", {
  expect_error(g2() %>% fig_error())

  df <- data.frame(
    x = as.factor(c(1:10, 1:10)),
    y = runif(20, 15, 25),
    grp = rep(c("A", "B"), each = 2)
  )

  df$ymin <- df$y - runif(20, 1, 3)
  df$ymax <- df$y + runif(20, 1, 3)

  g <- g2(df, asp(x = x, color = grp)) %>% 
    fig_error(
      asp(ymin = ymin, ymax = ymax, size = 10), 
      adjust("dodge")
    )
  
  expect_equal(g$x$views[[1]]$type, "interval")
})

test_that("Figures - contour", {
  expect_error(g2() %>% fig_contour())

  data(volcano)

  x <- 1:nrow(volcano)
  y <- 1:ncol(volcano)
  df <- expand.grid(x = x, y = y)
  df$z <- apply(df, 1, function(x) {
    volcano[x[1], x[2]]
  })

  g <- g2(df, asp(x, y, z = z)) %>% 
    fig_contour(colors = c("red", "blue"))

  expect_length(g$x$views, 20)

  g <- g2(df, asp(x, y, z = z)) %>% 
    fig_contour(type = "filled")

  expect_length(g$x$views, 20)
})

test_that("Figures - segment", {
  expect_error(g2() %>% fig_segment())

  seg <- df <- data.frame(
    x = c(24, 23),
    y = c(70, 54),
    xend = c(17, 24),
    yend = c(50, 93)
  )

  g <- g2(cars, asp(speed, dist)) %>% 
    fig_segment(
      asp(x = x, y = y, xend = xend, yend = yend),
      data = seg
    )

  expect_equal(g$x$views[[1]]$type, "edge")
})
