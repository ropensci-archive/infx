context("openbis create objects")

test_that("create PlateIdentifier objects", {
  expect_error(create_plate_id())
  expect_s3_class(create_plate_id("foo", "bar"), "json_class")
  expect_true(has_json_class(create_plate_id("foo", "bar"), "PlateIdentifier"))
  expect_error(create_plate_id("foo"))
  expect_error(create_plate_id("foo", token = tok))
  bc <- "KB2-03-1E"
  expect_s3_class(id <- create_plate_id(bc, token = tok), "json_class")
  expect_true(has_json_class(id, "PlateIdentifier"))
  expect_equal(id[["plateCode"]], bc)
  expect_equal(id[["spaceCodeOrNull"]], "INFECTX_PUBLISHED")
})
