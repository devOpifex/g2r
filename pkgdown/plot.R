library(g2r)

g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
  fig_point(asp(shape = Species)) %>% 
  planes(~Species, type = "tree")
