test_that("Tooltip", {
  g <- g2(cars, asp(speed, dist)) %>%
    fig_point() %>%
    tooltip(shared = TRUE)

  expect_equal(g$x$tooltip, list(shared = TRUE))

  template <- tpl(
    tpl_item(
      island,
      bill_depth_mm
    )
  )

  cb <- htmlwidgets::JS(
    "(island, bill_depth_mm) => {
      return {
        island: island,
        bill_depth_mm: bill_depth_mm
      };
    }"
  )

  g <- g2(cars, asp(speed, dist), elementId = "x") %>%
    fig_point(
      asp(
        shape = "circle",
        tooltip = island,
        tooltip = bill_depth_mm
      )
    ) %>%
    gauge_color(c("#0C59FE", "#FEC700", "#FC0F00")) %>%
    gauge_tooltip(cb) %>%
    tooltip(itemTpl = template)

  expect_type(g$x$tooltip$itemTpl, "character")
})
