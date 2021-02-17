test_that("Inputs", {
  expect_error(input_button())
  expect_error(input_button("id"))

  inp <- input_button("id", "label")
  expect_s3_class(inp, "shiny.tag")

  expect_error(input_slider())
  expect_error(input_slider("id"))
  expect_error(input_slider("id", "label"))
  expect_error(input_slider("id", "label", 0))
  expect_error(input_slider("id", "label", 0, 0))

  inp <- input_slider("id", "label", 0, 0, 5)
  expect_s3_class(inp, "shiny.tag")

  expect_error(input_select())
  expect_error(input_select("id"))
  expect_error(input_select("id", "label"))

  inp <- input_button("id", "label", letters[1:2])
  expect_s3_class(inp, "shiny.tag")

  expect_error(input_checkbox())
  expect_error(input_checkbox("id"))
  expect_error(input_checkbox("id", "label"))

  inp <- input_checkbox("id", "label", letters[1:2])
  expect_s3_class(inp, "shiny.tag")

  inp <- input_checkbox("id", "label", letters[1:2], "a")
  expect_s3_class(inp, "shiny.tag")

  inp <- input_checkbox("id", "label", letters[1:2], "a", FALSE)
  expect_s3_class(inp, "shiny.tag")

  expect_error(input_radio())
  expect_error(input_radio("id"))
  expect_error(input_radio("id", "label"))

  inp <- input_radio("id", "label", letters[1:2])
  expect_s3_class(inp, "shiny.tag")

  inp <- input_radio("id", "label", letters[1:2], "a")
  expect_s3_class(inp, "shiny.tag")

  inp <- input_radio("id", "label", letters[1:2], "a", FALSE)
  expect_s3_class(inp, "shiny.tag")
})
