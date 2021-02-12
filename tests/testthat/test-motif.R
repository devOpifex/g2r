test_that("Motif", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point()

  expect_snapshot_output(g)

  t <- motif(
    g, 
    background = "grey", 
    brandColor = "red",
    renderer = "svg",
    elementPoint(
      shape = "circle",
      stroke = 0,
      opacity = 1
    )
  )

  expect_equal(t$x$motif$background, "grey")
  expect_equal(t$x$motif$styleSheet$brandColor, "red")
  expect_length(t$x$motif$geometries, 1)
  expect_equal(t$x$chartOpts$renderer, "svg")

  theme_list <- list(
    background = "grey"
  )

  t2 <- motif_from_list(g, theme_list)
  expect_equal(t2$x$motif$background, "grey")

  t3 <- motif(
    g,
    elementArea(fill = "red"),
    elementEdge(color = "red"),
    elementInterval(fill = "red"),
    elementLine(fill = "red"),
    elementPolygon(fill = "red"),
    elementSchema(fill = "red")
  )

  expect_length(t3$x$motif$geometries, 6)
})
