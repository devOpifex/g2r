test_that("Event", {
  g <- g2(cars, asp(dist, speed)) %>% 
    fig_point() %>% 
    capture_event("point:click")

  expect_equal(g$x$events[[1]]$event, "point:click")
  expect_null(g$x$events[[1]]$callback)

  cb <- htmlwidgets::JS("() => { console.log('hello') }")

  g <- g2(cars, asp(dist, speed)) %>% 
    fig_point() %>% 
    capture_event("element:click", cb)

  expect_equal(g$x$events[[1]]$event, "element:click")
  expect_s3_class(g$x$events[[1]]$callback, "JS_EVAL")

  expect_error(capture_event(g, "event", "ERROR"))
})
