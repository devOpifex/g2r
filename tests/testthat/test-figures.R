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
