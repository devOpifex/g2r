test_that("Planes", {
  g <- g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
    fig_point() %>% 
    planes(~Species, type = "tree")

  expect_length(g$x$facet, 2)
})
