test_that("Motif", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() 

  expect_snapshot_output(g)
  
  t <- motif(g, background = "grey")

  expect_equal(t$x$motif$background, "grey")

})
