test_that("Graph", {
  ig <- igraph::make_ring(20)

  g <- g2(ig, asp(x, y)) %>%
    layout_igraph(method = igraph::layout_in_circle) %>% 
    fig_edge() %>% 
    fig_point(asp(shape = "circle")) %>% 
    axis_hide()

  expect_length(g$x$views, 2)
  expect_length(g$x$data, 2)

  g <- g2(ig, asp(x, y)) %>%
    layout_arc() %>% 
    fig_edge() %>% 
    fig_point(asp(shape = "circle")) %>% 
    axis_hide()

  expect_length(g$x$views, 2)
  expect_s3_class(g$x$graph, "igraph")
})
