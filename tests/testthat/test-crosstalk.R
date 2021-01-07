test_that("Crosstalk", {
  sd <- crosstalk::SharedData$new(cars)

  g <- g2(sd, asp(speed, dist)) %>% 
    fig_interval() %>% 
    crosstalk_select("fill", on = "green", off = "grey") %>% 
    crosstalk_select("stroke", on = "black", off = "grey")

  expect_length(g$x$crosstalk_select, 2)
  expect_equal(g$x$crosstalk_select[[1]]$attribute, "fill")
  expect_equal(g$x$crosstalk_select[[2]]$attribute, "stroke")
})
