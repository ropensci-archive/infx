context("dataset")

test_that("dataset types can be listed", {
  exp_type <- list_experiment_types(tok)
  expect_is(exp_type, "ExperimentType")
  expect_is(exp_type, "json_vec")
  expect_identical(get_common_subclass(exp_type), "ExperimentType")
  expect_true(all(sapply(exp_type, has_json_subclass, "ExperimentType")))
  expect_gte(length(exp_type), 1L)
})
