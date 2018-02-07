context("experiment")

test_that("experiments can be listed", {
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_identical(get_subclass(experiments), "Experiment")
  expect_true(all(sapply(experiments, has_subclass, "Experiment")))
  expect_equal(length(experiments), 2L)

  experiments <- list_experiments(tok, exp_ids[[1]])
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_identical(get_subclass(experiments), "Experiment")
  expect_true(all(sapply(experiments, has_subclass, "Experiment")))
  expect_equal(length(experiments), 1L)

  expect_error(list_experiments(tok,
                                json_class(a = 1, b = 2,
                                           class = "ExperimentIdentifier")))

  experiments <- list_experiments(tok, projects[1:2],
                                  json_class(code = "SIRNA_HCS",
                                             class = "ExperimentType"))
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_identical(get_subclass(experiments), "Experiment")
  expect_true(all(sapply(experiments, has_subclass, "Experiment")))
  expect_gte(length(experiments), 1L)

  experiments <- list_experiments(tok, projects[[1]])
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_identical(get_subclass(experiments), "Experiment")
  expect_true(all(sapply(experiments, has_subclass, "Experiment")))
  expect_gte(length(experiments), 1L)

  expect_identical(list_experiments(tok, projects[[1]],
                                    json_class(code = "SIRNA_HCS",
                                               class = "ExperimentType"),
                                    "DataSets"),
                   list_experiments(tok, projects[[1]],
                                    json_class(code = "SIRNA_HCS",
                                               class = "ExperimentType"),
                                    "Samples"))
})

test_that("experiment types can be listed", {
  exp_type <- list_experiment_types(tok)
  expect_is(exp_type, "ExperimentType")
  expect_is(exp_type, "json_vec")
  expect_identical(get_subclass(exp_type), "ExperimentType")
  expect_true(all(sapply(exp_type, has_subclass, "ExperimentType")))
  expect_gte(length(exp_type), 1L)
})

test_that("experiment ids can be listed", {
  expect_is(exp_ids, "ExperimentIdentifier")
  expect_is(exp_ids, "json_vec")
  expect_identical(get_subclass(exp_ids), "ExperimentIdentifier")
  expect_true(all(sapply(exp_ids, has_subclass, "ExperimentIdentifier")))
  expect_gte(length(exp_ids), 1L)
})
