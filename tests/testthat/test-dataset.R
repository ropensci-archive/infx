context("dataset")

test_that("datasets can be listed", {
  ds_1 <- datasets
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)
  expect_true(all(sapply(lapply(ds_1, `[[`, "retrievedConnections"),
                         identical, list())))

  check_skip()

  ds_2 <- list_datasets(tok, samples[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"),
                         identical, list())))

  ds_3 <- list_datasets(tok, samples[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_1))
  expect_true(all(sapply(lapply(ds_3, `[[`, "retrievedConnections"),
                         identical, list("PARENTS", "CHILDREN"))))

  ds_1 <- list_datasets(tok, experiments[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_subclass, "DataSet")))
  expect_gte(length(ds_1), 1L)
  expect_true(all(sapply(lapply(ds_1, `[[`, "retrievedConnections"),
                         identical, list())))

  ds_2 <- list_datasets(tok, experiments[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_subclass, "DataSet")))
  expect_gte(length(ds_2), length(ds_1))
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"),
                         identical, list())))

  ds_3 <- list_datasets(tok, experiments[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_subclass, "DataSet")))
  expect_gte(length(ds_3), length(ds_1))
  expect_true(all(sapply(lapply(ds_3, `[[`, "retrievedConnections"),
                         identical, list("PARENTS", "CHILDREN"))))

  codes <- sapply(ds_3, `[[`, "code")

  ds_1 <- list_datasets(tok, codes[1])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_true(all(sapply(ds_1, has_subclass, "DataSet")))
  expect_equal(length(ds_1), 1L)
  expect_equal(ds_1[[1]][["retrievedConnections"]], list())

  ds_2 <- list_datasets(tok, codes[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "DataSet")
  expect_true(all(sapply(ds_2, has_subclass, "DataSet")))
  expect_equal(length(ds_2), 2L)
  expect_true(all(sapply(lapply(ds_2, `[[`, "retrievedConnections"),
                         identical, list())))

  ds_3 <- list_datasets(tok, codes[1], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_identical(get_subclass(ds_3), "DataSet")
  expect_true(all(sapply(ds_3, has_subclass, "DataSet")))
  expect_equal(length(ds_3), 1L)
  expect_identical(ds_3[[1]][["retrievedConnections"]],
                   list("PARENTS", "CHILDREN"))
})

test_that("dataset references can be listed", {

  check_skip()

  mat <- material_id(c(3832L, 2475L), mode = "screening")

  ds_1 <- list_references(tok, mat[[1]], exp_ids[[1]])
  expect_identical(ds_1, list())

  ds_1 <- list_references(tok, mat[[2]], exp_ids[[1]])
  expect_is(ds_1, "PlateWellReferenceWithDatasets")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ds_1, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ds_1), 1L)
  img_ref <- ds_1[[1]][["imageDatasetReferences"]][[1]]
  expect_is(img_ref, "ImageDatasetReference")
  expect_is(img_ref, "json_class")

  ds_2 <- list_references(tok, mat[1:2], exp_ids[[1]])
  expect_is(ds_2, "PlateWellReferenceWithDatasets")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ds_2, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ds_2), length(ds_1))

  ds_1 <- list_references(tok, samples[[1]])
  expect_is(ds_1, "ImageDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "ImageDatasetReference")
  expect_true(all(sapply(ds_1, has_subclass, "ImageDatasetReference")))
  expect_equal(length(ds_1), 1L)

  ds_2 <- list_references(tok, samples[1:2])
  expect_is(ds_2, "ImageDatasetReference")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "ImageDatasetReference")
  expect_true(all(sapply(ds_2, has_subclass, "ImageDatasetReference")))
  expect_equal(length(ds_2), 2L)

  ds_1 <- list_references(tok, plates[[1]])
  expect_is(ds_1, "ImageDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "ImageDatasetReference")
  expect_true(all(sapply(ds_1, has_subclass, "ImageDatasetReference")))
  expect_equal(length(ds_1), 1L)

  ds_2 <- list_references(tok, plates[1:2])
  expect_is(ds_2, "ImageDatasetReference")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "ImageDatasetReference")
  expect_true(all(sapply(ds_2, has_subclass, "ImageDatasetReference")))
  expect_equal(length(ds_2), 2L)

  plate_ids <- as_plate_id(plates)

  expect_identical(list_references(tok, plate_ids[[1]]), ds_1)
  expect_identical(list_references(tok, plate_ids[1:2]), ds_2)

  expect_identical(list_references(tok, plate_meta[[1]]), ds_1)
  expect_identical(list_references(tok, plate_meta[1:2]), ds_2)

  ds_1 <- list_references(tok, plates[[1]], type = "feature")
  expect_is(ds_1, "FeatureVectorDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "FeatureVectorDatasetReference")
  expect_true(all(sapply(ds_1, has_subclass, "FeatureVectorDatasetReference")))
  expect_gte(length(ds_1), 1L)

  dsids <- list_dataset_ids(tok, sapply(ds_2, `[[`, "datasetCode"))

  ds_1 <- list_references(tok, dsids[[1]], channels = "DAPI")
  expect_is(ds_1, "MicroscopyImageReference")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "MicroscopyImageReference")
  expect_true(all(sapply(ds_1, has_subclass, "MicroscopyImageReference")))
  expect_equal(length(ds_1), 9L)

  ds_2 <- list_references(tok, dsids[1:2], channels = "DAPI")
  expect_is(ds_2, "MicroscopyImageReference")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "MicroscopyImageReference")
  expect_true(all(sapply(ds_2, has_subclass, "MicroscopyImageReference")))
  expect_equal(length(ds_2), 18L)

  ds_3 <- list_references(tok, dsids[[1]], channels = c("DAPI", "GFP"))
  expect_is(ds_3, "MicroscopyImageReference")
  expect_is(ds_3, "json_vec")
  expect_identical(get_subclass(ds_3), "MicroscopyImageReference")
  expect_true(all(sapply(ds_3, has_subclass, "MicroscopyImageReference")))
  expect_equal(length(ds_3), 18L)

  # check Dataset
  expect_identical(list_references(tok,
                                   list_datasets(tok,
                                                 ds_1[[1]][["datasetCode"]]),
                                   channels = "DAPI"), ds_1)
  # check MicroscopyImageReference
  expect_identical(list_references(tok, ds_1[[1]], channels = "DAPI"), ds_1)
  # check ImageDatasetReference
  expect_identical(list_references(tok, list_references(tok, plates[[1]]),
                                   channels = "DAPI"), ds_1)

  well_pos <- as_json_vec(lapply(wells[1:2], `[[`, "wellPosition"))
  ds_1 <- list_references(tok, dsids[[1]], wells = well_pos[[1]],
                          channels = "DAPI")
  expect_is(ds_1, "PlateImageReference")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "PlateImageReference")
  expect_true(all(sapply(ds_1, has_subclass, "PlateImageReference")))
  expect_equal(length(ds_1), 9L)

  ds_2 <- list_references(tok, dsids[[1]], wells = well_pos, channels = "DAPI")
  expect_is(ds_2, "PlateImageReference")
  expect_is(ds_2, "json_vec")
  expect_identical(get_subclass(ds_2), "PlateImageReference")
  expect_true(all(sapply(ds_2, has_subclass, "PlateImageReference")))
  expect_equal(length(ds_2), 18L)

  ds_3 <- list_references(tok, dsids[[1]], wells = well_pos[[1]],
                          channels = c("DAPI", "GFP"))
  expect_is(ds_3, "PlateImageReference")
  expect_is(ds_3, "json_vec")
  expect_identical(get_subclass(ds_3), "PlateImageReference")
  expect_true(all(sapply(ds_3, has_subclass, "PlateImageReference")))
  expect_equal(length(ds_3), 18L)
})

test_that("dataset types can be listed", {

  check_skip()

  ds_types <- list_dataset_types(tok)
  expect_is(ds_types, "DataSetType")
  expect_is(ds_types, "json_vec")
  expect_identical(get_subclass(ds_types), "DataSetType")
  expect_true(all(sapply(ds_types, has_subclass, "DataSetType")))
  expect_gte(length(ds_types), 1L)
})

test_that("dataset ids can be listed", {

  check_skip()

  codes <- dataset_code(datasets)

  dsid_1 <- list_dataset_ids(tok, codes[[1]])
  expect_is(dsid_1, "DatasetIdentifier")
  expect_is(dsid_1, "json_vec")
  expect_identical(get_subclass(dsid_1), "DatasetIdentifier")
  expect_true(all(sapply(dsid_1, has_subclass, "DatasetIdentifier")))
  expect_gte(length(dsid_1), 1L)

  dsid_2 <- list_dataset_ids(tok, codes[1:2])
  expect_is(dsid_2, "DatasetIdentifier")
  expect_is(dsid_2, "json_vec")
  expect_identical(get_subclass(dsid_2), "DatasetIdentifier")
  expect_true(all(sapply(dsid_2, has_subclass, "DatasetIdentifier")))
  expect_gte(length(dsid_2), 1L)

  expect_identical(list_dataset_ids(tok, datasets[[1]]), dsid_1)
  expect_identical(list_dataset_ids(tok, datasets[1:2]), dsid_2)
})

test_that("dataset codes can be extracted", {

  check_skip()

  codes <- dataset_code(datasets)
  expect_is(codes, "character")
  expect_length(codes, length(datasets))

  codes <- dataset_code(list_dataset_ids(tok, codes[1:2]))
  expect_is(codes, "character")
  expect_length(codes, 2L)
})
