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

test_that("create ExperimentIdentifier objects", {
  expect_error(create_exp_ids())
  expect_equal(create_exp_ids(token = tok), list_experiment_ids(tok))
  expect_equal(create_exp_ids(exp_code = "VACCINIA-QU-K1",
                              proj_code = "VACCINIA_TEAM",
                              space_code = "INFECTX_PUBLISHED",
                              perm_id = "20121218143856336-1733311"),
               create_exp_ids(perm_id = "20121218143856336-1733311",
                              token = tok, fixed = TRUE))
  expect_is(exp_ids <- create_exp_ids(exp_code = "BRUCELLA-AU-K[12]",
                                      token = tok), "list")
  expect_equal(length(exp_ids), 2L)
  expect_true(all(sapply(exp_ids, has_json_class, "ExperimentIdentifier")))
})
