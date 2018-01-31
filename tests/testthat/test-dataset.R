context("dataset")

test_that("datasets can be listed", {
  exp_ids <- list_experiment_ids(tok)
  samples <- list_samples(tok, exp_ids[c(1, 2)])

  ds_1 <- list_datasets(tok, samples[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)
  expect_true(all(sapply(lapply(ds_1, `[[`, "retrievedConnections"), is.null)))

  ds_2 <- list_datasets(tok, samples[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"), is.null)))

  ds_3 <- list_datasets(tok, samples[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_1))
  expect_true(all(sapply(lapply(ds_3, `[[`, "retrievedConnections"),
                         identical, list("PARENTS", "CHILDREN"))))

  experiments <- list_experiments(tok, exp_ids[1:2])

  ds_1 <- list_datasets(tok, experiments[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)
  expect_true(all(sapply(lapply(ds_1, `[[`, "retrievedConnections"), is.null)))

  ds_2 <- list_datasets(tok, experiments[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"), is.null)))

  ds_3 <- list_datasets(tok, experiments[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_1))
  expect_true(all(sapply(lapply(ds_3, `[[`, "retrievedConnections"),
                         identical, list("PARENTS", "CHILDREN"))))

  codes <- sapply(ds_3, `[[`, "code")

  ds_1 <- list_datasets(tok, codes[1])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_subclass, "DataSet")))
  expect_equal(length(ds_1), 1L)
  expect_true(is.null(ds_1[[1]][["retrievedConnections"]]))

  ds_2 <- list_datasets(tok, codes[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_subclass, "DataSet")))
  expect_equal(length(ds_2), 2L)
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"), is.null)))

  ds_3 <- list_datasets(tok, codes[1], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_common_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_subclass, "DataSet")))
  expect_equal(length(ds_3), 1L)
  expect_identical(ds_3[[1]][["retrievedConnections"]],
                   list("PARENTS", "CHILDREN"))

  plates <- list_plates(tok, exp_ids[[1]])
  plate_ids <- plate_to_plateid(plates)
  meta <- list_plate_metadata(tok, plate_ids[1:2])

  ds_1 <- list_datasets(tok, plates[[1]])
  expect_is(ds_1, "ImageDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1), "ImageDatasetReference")
  expect_true(all(sapply(ds_1, has_subclass, "ImageDatasetReference")))
  expect_equal(length(ds_1), 1L)

  ds_2 <- list_datasets(tok, plates[1:2])
  expect_is(ds_2, "ImageDatasetReference")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2), "ImageDatasetReference")
  expect_true(all(sapply(ds_2, has_subclass, "ImageDatasetReference")))
  expect_equal(length(ds_2), 2L)

  expect_identical(list_datasets(tok, plate_ids[[1]]), ds_1)
  expect_identical(list_datasets(tok, plate_ids[1:2]), ds_2)

  expect_identical(list_datasets(tok, meta[[1]]), ds_1)
  expect_identical(list_datasets(tok, meta[1:2]), ds_2)

  mat <- material_id(c(2475L, 3832L), mode = "screening")

  ds_1 <- list_datasets(tok, mat[[1]], exp_ids[[1]])
  expect_is(ds_1, "PlateWellReferenceWithDatasets")
  expect_is(ds_1, "json_vec")
  expect_identical(get_common_subclass(ds_1),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ds_1, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ds_1), 1L)
  img_ref <- ds_1[[1]][["imageDatasetReferences"]][[1]]
  expect_is(img_ref, "ImageDatasetReference")
  expect_is(img_ref, "json_class")

  ds_2 <- list_datasets(tok, mat[1:2], exp_ids[[1]])
  expect_is(ds_2, "PlateWellReferenceWithDatasets")
  expect_is(ds_2, "json_vec")
  expect_identical(get_common_subclass(ds_2),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ds_2, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ds_2), length(ds_1))
})

test_that("dataset types can be listed", {
  ds_types <- list_dataset_types(tok)
  expect_is(ds_types, "DataSetType")
  expect_is(ds_types, "json_vec")
  expect_identical(get_common_subclass(ds_types), "DataSetType")
  expect_true(all(sapply(ds_types, has_subclass, "DataSetType")))
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
  expect_true(all(sapply(dsid_1, has_subclass, "DatasetIdentifier")))
  expect_gte(length(dsid_1), 1L)

  dsid_2 <- list_dataset_ids(tok, codes[1:2])
  expect_is(dsid_2, "DatasetIdentifier")
  expect_is(dsid_2, "json_vec")
  expect_identical(get_common_subclass(dsid_2), "DatasetIdentifier")
  expect_true(all(sapply(dsid_2, has_subclass, "DatasetIdentifier")))
  expect_gte(length(dsid_2), 1L)

  expect_identical(list_dataset_ids(tok, ds[[1]]), dsid_1)
  expect_identical(list_dataset_ids(tok, ds[1:2]), dsid_2)
})
