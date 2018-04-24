context("experiment")

test_that("experiments can be listed", {

  exps <- experiments
  expect_s3_class(exps, "Experiment")
  expect_s3_class(exps, "json_vec")
  expect_length(exps, 2L)
  for (i in seq_along(exps)) {
    expect_s3_class(exps[[i]], "Experiment")
    expect_s3_class(exps[[i]], "json_class")
    expect_true(has_fields(exps[[i]], "registrationDetails"))
    expect_s3_class(exps[[i]][["registrationDetails"]],
                    "EntityRegistrationDetails")
    expect_s3_class(exps[[i]][["registrationDetails"]], "json_class")
  }

  check_skip()

  exps <- list_experiments(tok, exp_ids[[1]])
  expect_s3_class(exps, "Experiment")
  expect_s3_class(exps, "json_class")
  expect_true(has_fields(exps, "registrationDetails"))
  expect_s3_class(exps[["registrationDetails"]],
                  "EntityRegistrationDetails")
  expect_s3_class(exps[["registrationDetails"]], "json_class")

  expect_error(list_experiments(tok,
                                json_class(a = 1, b = 2,
                                           class = "ExperimentIdentifier")))

  exps <- list_experiments(tok, projects[1:2],
                                  json_class(code = "SIRNA_HCS",
                                             class = "ExperimentType"))
  expect_s3_class(exps, "Experiment")
  expect_s3_class(exps, "json_vec")
  expect_gte(length(exps), 1L)
  for (i in seq_along(exps)) {
    expect_s3_class(exps[[i]], "Experiment")
    expect_s3_class(exps[[i]], "json_class")
    expect_true(has_fields(exps[[i]], "registrationDetails"))
    expect_s3_class(exps[[i]][["registrationDetails"]],
                    "EntityRegistrationDetails")
    expect_s3_class(exps[[i]][["registrationDetails"]], "json_class")
  }

  exps <- list_experiments(tok, projects[[1]])
  expect_s3_class(exps, "Experiment")
  expect_s3_class(exps, "json_vec")
  expect_gte(length(exps), 1L)
  for (i in seq_along(exps)) {
    expect_s3_class(exps[[i]], "Experiment")
    expect_s3_class(exps[[i]], "json_class")
    expect_true(has_fields(exps[[i]], "registrationDetails"))
    expect_s3_class(exps[[i]][["registrationDetails"]],
                    "EntityRegistrationDetails")
    expect_s3_class(exps[[i]][["registrationDetails"]], "json_class")
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
  expect_s3_class(exp_type, "ExperimentType")
  expect_s3_class(exp_type, "json_vec")
  expect_gte(length(exp_type), 1L)
  for (i in seq_along(exp_type)) {
    expect_s3_class(exp_type[[i]], "ExperimentType")
    expect_s3_class(exp_type[[i]], "json_class")
  }
})

test_that("experiment ids can be listed", {
  expect_s3_class(exp_ids, "ExperimentIdentifier")
  expect_s3_class(exp_ids, "json_vec")
  expect_gte(length(exp_ids), 1L)
  for (i in seq_along(exp_ids)) {
    expect_s3_class(exp_ids[[i]], "ExperimentIdentifier")
    expect_s3_class(exp_ids[[i]], "json_class")
  }
})
