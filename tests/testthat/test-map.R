test_that("GADM data", {
  expect_error(get_gadm_data())

  simp <- get_gadm_data("BEL")
  expect_s4_class(simp, "SpatialPolygonsDataFrame")

  full <- get_gadm_data("BEL", keep = NULL)
  expect_s4_class(full, "SpatialPolygonsDataFrame")

  expect_gt(object.size(full), object.size(simp))
})
