test_that("Interplay", {
  g <- g2(cars, asp(dist, speed)) %>% 
    fig_line() %>% 
    interplay("brush-visible") %>% 
    remove_interplay("tooltip")
  
  expect_equal(g$x$interactions, list("brush-visible"))
  expect_equal(g$x$rmInteractions, list("tooltip"))

  g <- g2(cars, asp(speed, dist, color = dist)) %>% 
    fig_interval(
      asp(interplay = "element-highlight-by-color"),
      adjust("dodge")
    ) %>%  
    register_interplay(
      "element-highlight-by-color",
        start = list(
          list(
            trigger = 'element:mouseenter', 
            action = 'element-highlight-by-color:highlight'
          )
        ),
        end = list(
          list(
            trigger = 'element:mouseleave', 
            action = 'element-highlight-by-color:reset'
          )
        )
      )

  expect_length(g$x$registerInteractions, 1)
})
