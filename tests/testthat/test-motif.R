test_that("Motif", {
  g <- g2(cars, asp(speed, dist)) %>% 
    fig_point() 

  expect_snapshot_output(g)
  
  t <- motif(g, background = "grey")

  expect_equal(t$x$theme, list(background = "grey"))

  d <- motif(g, name = "dark")
  expect_equal(d$x$chartOpts$theme, "dark")

  motif_global(name = "dark")

  g_motif_name <- g2(cars, asp(speed, dist), elementId = "x1") %>% 
    fig_point() 

  expect_snapshot_output(g_motif_name)

  motif_global(background = "darkgrey")
  expect_equal(getOption("G2_CHART_OPTS")$theme, "custom")
  expect_equal(getOption("G2_THEME"), list(background = "darkgrey"))

  g_motif <- g2(cars, asp(speed, dist), elementId = "x") %>% 
    fig_point() 

  expect_snapshot_output(g_motif)

  motif_global_reset()
})
