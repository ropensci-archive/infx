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
  expect_true(all(sapply(lapply(ds_1, `[[`, "retrievedConnections"), is.null)))

  ds_2 <- list_datasets(tok, samples[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_json_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"), is.null)))

  ds_3 <- list_datasets(tok, samples[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_json_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_1))
  expect_true(all(sapply(lapply(ds_3, `[[`, "retrievedConnections"),
                         identical, list("PARENTS", "CHILDREN"))))

  experiments <- list_experiments(tok, exp_ids[1:2])

  ds_1 <- list_datasets(tok, experiments[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_json_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)
  expect_true(all(sapply(lapply(ds_1, `[[`, "retrievedConnections"), is.null)))

  ds_2 <- list_datasets(tok, experiments[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_json_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"), is.null)))

  ds_3 <- list_datasets(tok, experiments[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_json_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_1))
  expect_true(all(sapply(lapply(ds_3, `[[`, "retrievedConnections"),
                         identical, list("PARENTS", "CHILDREN"))))

  codes <- sapply(ds_3, `[[`, "code")

  ds_1 <- list_datasets(tok, codes[1])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_json_subclass, "DataSet")))
  expect_equal(length(ds_1), 1L)
  expect_true(is.null(ds_1[[1]][["retrievedConnections"]]))

  ds_2 <- list_datasets(tok, codes[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_json_subclass, "DataSet")))
  expect_equal(length(ds_2), 2L)
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"), is.null)))

  ds_3 <- list_datasets(tok, codes[1], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_json_subclass, "DataSet")))
  expect_equal(length(ds_3), 1L)
  expect_identical(ds_3[[1]][["retrievedConnections"]],
                   list("PARENTS", "CHILDREN"))
})

test_that("dataset types can be listed", {
  ds_types <- list_dataset_types(tok)
  expect_is(ds_types, "DataSetType")
  expect_is(ds_types, "json_vec")
  expect_identical(get_common_subclass(ds_types), "DataSetType")
  expect_true(all(sapply(ds_types, has_json_subclass, "DataSetType")))
  expect_gte(length(ds_types), 1L)
})

test_that("dataset ids can be listed", {
  exp_ids <- list_experiment_ids(tok)
  samples <- list_samples(tok, exp_ids[[1]])
  ds <- list_datasets(tok, samples[[1]])
  codes <- sapply(ds, `[[`, "code")

  dsid_1 <- list_dataset_ids(tok, codes[[1]])
  expect_is(dsid_1, "DatasetIdentifier")
  expect_is(dsid_1, "json_vec")
  expect_identical(get_common_subclass(dsid_1), "DatasetIdentifier")
  expect_true(all(sapply(dsid_1, has_json_subclass, "DatasetIdentifier")))
  expect_gte(length(dsid_1), 1L)

  dsid_2 <- list_dataset_ids(tok, codes[1:2])
  expect_is(dsid_2, "DatasetIdentifier")
  expect_is(dsid_2, "json_vec")
  expect_identical(get_common_subclass(dsid_2), "DatasetIdentifier")
  expect_true(all(sapply(dsid_2, has_json_subclass, "DatasetIdentifier")))
  expect_gte(length(dsid_2), 1L)

  expect_identical(list_dataset_ids(tok, ds[[1]]), dsid_1)
  expect_identical(list_dataset_ids(tok, ds[1:2]), dsid_2)
})
