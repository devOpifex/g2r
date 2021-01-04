test_that("g2 default", {
  g <- g2()
  expect_null(g$x$data)
  expect_length(g$x$main_asp, 0)
  
  g <- g2(asp(dist))
  expect_null(g$x$data)
  expect_length(g$x$main_asp, 1)

  g <- g2(cars, asp(dist))
  expect_length(g$x$data, 2)
  expect_length(g$x$main_asp, 1)
})

test_that("g2 igraph", {
  ig <- igraph::make_ring(10)

  g <- g2(ig)
  expect_length(g$x$data, 2)
  expect_length(g$x$graph, 10)
  
})
