test_that("Init default", {
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

test_that("Init igraph", {
  ig <- igraph::make_ring(10)

  g <- g2(ig)
  expect_length(g$x$data, 2)
  expect_length(g$x$graph, 10)
  
})

test_that("Init crosstalk", {
  sd <- crosstalk::SharedData$new(cars, group = "grp")

  g <- g2(sd, asp(speed, dist)) %>% 
    fig_point()
  
  expect_equal(g$x$crosstalk_group, "grp")
  expect_equal(names(g$x$data), c("speed", "dist", "CROSSTALK_KEYS"))
})

test_that("Init output", {
  g <- g2(cars, asp(speed, dist), elementId = "x") %>% 
    fig_point()
  
  expect_snapshot_output(g)
})
