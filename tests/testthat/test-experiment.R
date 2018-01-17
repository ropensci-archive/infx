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

test_that("experiments can be listed", {
  exp_ids <- list_experiment_ids(tok)
  experiments <- list_experiments(exp_ids[1:2], tok)

  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_identical(get_common_subclass(experiments), "Experiment")
  expect_true(all(sapply(experiments, has_json_subclass, "Experiment")))
  expect_equal(length(experiments), 2L)

  experiments <- list_experiments(exp_ids[[1]], tok)
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_identical(get_common_subclass(experiments), "Experiment")
  expect_true(all(sapply(experiments, has_json_subclass, "Experiment")))
  expect_equal(length(experiments), 1L)
})
