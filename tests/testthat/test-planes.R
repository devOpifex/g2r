test_that("Planes", {
  g <- g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>%
    fig_point() %>%
    planes(~Species, type = "tree")

  expect_length(g$x$facet, 2)

  expect_error(g2(cars) %>% planes())
  expect_error(g2(cars) %>% planes("err"))
  expect_error(g2() %>% planes(~x))
})
