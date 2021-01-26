test_that("Gauge error", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(color = dist))

  expect_error(gauge_interplay(g))
})

test_that("Gauge color", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(color = dist))

  # size not defined, nothing happens
  ident <- gauge_size(g, "something")
  expect_identical(g, ident)

  color <- gauge_color(g, c("red", "blue"))
  expect_length(color$x$views[[1]]$color, 2)
  expect_equal(color$x$views[[1]]$color[[2]], c("red", "blue"))
})

test_that("Gauge size", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(size = dist))

  cb <- "function(val){
    if(val < 60)
      return 6;
    
    return 3;
  }"

  size <- gauge_size(g, htmlwidgets::JS(cb))
  expect_length(size$x$views[[1]]$size, 2)
})

test_that("Gauge shape", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(shape = dist))

  expect_length(g$x$views[[1]]$shape, 1)

  cb <- "function(val){
    if(val < 40)
      return 'square';
    
    return 'circle';
  }"

  shape <- gauge_shape(g, htmlwidgets::JS(cb))
  expect_length(shape$x$views[[1]]$shape, 2)
})

test_that("Gauge label", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(label = dist))

  expect_length(g$x$views[[1]]$label, 1)

  lab <- gauge_label(g, style = list(fill = "red"))
  expect_length(lab$x$views[[1]]$label, 2)
})

test_that("Gauge style", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(style = dist))

  expect_length(g$x$views[[1]]$style, 1)

  cb <- "function(val){
    if(val < 40)
      return { fill: 'green' };
    
    return { fill: 'red' };
  }"

  style <- gauge_style(g, htmlwidgets::JS(cb))
  expect_length(style$x$views[[1]]$style, 2)
})

test_that("Gauge interplay", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point(asp(style = dist, interplay = "brush"))

  inter <- gauge_interplay(g, FALSE)
  expect_false(inter$x$views[[1]]$interaction)
})
