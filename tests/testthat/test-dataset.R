context("dataset")

test_that("datasets can be listed", {
  exp_ids <- list_experiment_ids(tok)
  samples <- list_samples(tok, exp_ids[c(1, 2)])

  ds_1 <- list_datasets(tok, samples[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_json_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)

  ds_2 <- list_datasets(tok, samples[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_json_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))

  ds_3 <- list_datasets(tok, samples[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_json_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_2))

  experiments <- list_experiments(tok, exp_ids[1:2])

  ds_1 <- list_datasets(tok, experiments[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_json_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)

  ds_2 <- list_datasets(tok, experiments[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_json_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))

  ds_3 <- list_datasets(tok, experiments[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_json_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_2))
})


test_that("dataset types can be listed", {
  ds_types <- list_dataset_types(tok)
  expect_is(ds_types, "DataSetType")
  expect_is(ds_types, "json_vec")
  expect_identical(get_common_subclass(ds_types), "DataSetType")
  expect_true(all(sapply(ds_types, has_json_subclass, "DataSetType")))
  expect_gte(length(ds_types), 1L)
})
