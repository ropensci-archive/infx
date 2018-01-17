context("experiment")

test_that("experiment types can be listed", {
  exp_type <- list_experiment_types(tok)
  expect_is(exp_type, "ExperimentType")
  expect_is(exp_type, "json_vec")
  expect_identical(get_common_subclass(exp_type), "ExperimentType")
  expect_true(all(sapply(exp_type, has_json_subclass, "ExperimentType")))
  expect_gte(length(exp_type), 1L)
})

test_that("experiment ids can be listed", {
  exp_ids <- list_experiment_ids(tok)
  expect_is(exp_ids, "ExperimentIdentifier")
  expect_is(exp_ids, "json_vec")
  expect_identical(get_common_subclass(exp_ids), "ExperimentIdentifier")
  expect_true(all(sapply(exp_ids, has_json_subclass, "ExperimentIdentifier")))
  expect_gte(length(exp_ids), 1L)
})
