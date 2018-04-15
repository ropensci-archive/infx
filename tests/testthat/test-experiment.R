context("experiment")

test_that("experiments can be listed", {
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_length(experiments, 2L)
  for (i in seq_along(experiments)) {
    expect_is(experiments[[i]], "Experiment")
    expect_is(experiments[[i]], "json_class")
  }

  check_skip()

  experiments <- list_experiments(tok, exp_ids[[1]])
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_length(experiments, 1L)
  expect_is(experiments[[1L]], "Experiment")
  expect_is(experiments[[1L]], "json_class")

  expect_error(list_experiments(tok,
                                json_class(a = 1, b = 2,
                                           class = "ExperimentIdentifier")))

  experiments <- list_experiments(tok, projects[1:2],
                                  json_class(code = "SIRNA_HCS",
                                             class = "ExperimentType"))
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_gte(length(experiments), 1L)
  for (i in seq_along(experiments)) {
    expect_is(experiments[[i]], "Experiment")
    expect_is(experiments[[i]], "json_class")
  }

  experiments <- list_experiments(tok, projects[[1]])
  expect_is(experiments, "Experiment")
  expect_is(experiments, "json_vec")
  expect_gte(length(experiments), 1L)
  for (i in seq_along(experiments)) {
    expect_is(experiments[[i]], "Experiment")
    expect_is(experiments[[i]], "json_class")
  }

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

  check_skip()

  exp_type <- list_experiment_types(tok)
  expect_is(exp_type, "ExperimentType")
  expect_is(exp_type, "json_vec")
  expect_gte(length(exp_type), 1L)
  for (i in seq_along(exp_type)) {
    expect_is(exp_type[[i]], "ExperimentType")
    expect_is(exp_type[[i]], "json_class")
  }
})

test_that("experiment ids can be listed", {
  expect_is(exp_ids, "ExperimentIdentifier")
  expect_is(exp_ids, "json_vec")
  expect_gte(length(exp_ids), 1L)
  for (i in seq_along(exp_type)) {
    expect_is(exp_ids[[i]], "ExperimentIdentifier")
    expect_is(exp_ids[[i]], "json_class")
  }
})
