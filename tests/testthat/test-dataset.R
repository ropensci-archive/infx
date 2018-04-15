context("dataset")

test_that("datasets can be listed", {
  ds_1 <- datasets
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_gte(length(ds_1), 1L)
  for (i in seq_along(ds_1)) {
    expect_is(ds_1[[i]], "DataSet")
    expect_is(ds_1[[i]], "json_class")
    expect_true(has_fields(ds_1[[i]], "retrievedConnections"))
    expect_identical(ds_1[[i]][["retrievedConnections"]], list())
  }

  check_skip()

  ds_2 <- list_datasets(tok, samples[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_gte(length(ds_2), length(ds_1))
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "DataSet")
    expect_is(ds_2[[i]], "json_class")
    expect_true(has_fields(ds_2[[i]], "retrievedConnections"))
    expect_identical(ds_2[[i]][["retrievedConnections"]], list())
  }

  ds_3 <- list_datasets(tok, samples[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_gte(length(ds_3), length(ds_1))
  for (i in seq_along(ds_3)) {
    expect_is(ds_3[[i]], "DataSet")
    expect_is(ds_3[[i]], "json_class")
    expect_true(has_fields(ds_3[[i]], "retrievedConnections"))
    expect_identical(ds_3[[i]][["retrievedConnections"]],
                     list("PARENTS", "CHILDREN"))
  }

  ds_1 <- list_datasets(tok, experiments[[1]])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_gte(length(ds_1), 1L)
  for (i in seq_along(ds_1)) {
    expect_is(ds_1[[i]], "DataSet")
    expect_is(ds_1[[i]], "json_class")
    expect_true(has_fields(ds_1[[i]], "retrievedConnections"))
    expect_identical(ds_1[[i]][["retrievedConnections"]], list())
  }

  ds_2 <- list_datasets(tok, experiments[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_gte(length(ds_2), length(ds_1))
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "DataSet")
    expect_is(ds_2[[i]], "json_class")
    expect_true(has_fields(ds_2[[i]], "retrievedConnections"))
    expect_identical(ds_2[[i]][["retrievedConnections"]], list())
  }

  ds_3 <- list_datasets(tok, experiments[[1]], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_gte(length(ds_3), length(ds_1))
  for (i in seq_along(ds_3)) {
    expect_is(ds_3[[i]], "DataSet")
    expect_is(ds_3[[i]], "json_class")
    expect_true(has_fields(ds_3[[i]], "retrievedConnections"))
    expect_identical(ds_3[[i]][["retrievedConnections"]],
                     list("PARENTS", "CHILDREN"))
  }

  codes <- sapply(ds_3, `[[`, "code")

  ds_1 <- list_datasets(tok, codes[1])
  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_length(ds_1, 1L)
  expect_is(ds_1[[1L]], "DataSet")
  expect_is(ds_1[[1L]], "json_class")
  expect_true(has_fields(ds_1[[1L]], "retrievedConnections"))
  expect_identical(ds_1[[1L]][["retrievedConnections"]], list())

  ds_2 <- list_datasets(tok, codes[c(1, 2)])
  expect_is(ds_2, "DataSet")
  expect_is(ds_2, "json_vec")
  expect_length(ds_2, 2L)
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "DataSet")
    expect_is(ds_2[[i]], "json_class")
    expect_true(has_fields(ds_2[[i]], "retrievedConnections"))
    expect_identical(ds_2[[i]][["retrievedConnections"]], list())
  }

  ds_3 <- list_datasets(tok, codes[1], "all")
  expect_is(ds_3, "DataSet")
  expect_is(ds_3, "json_vec")
  expect_length(ds_3, 1L)
  for (i in seq_along(ds_3)) {
    expect_is(ds_3[[i]], "DataSet")
    expect_is(ds_3[[i]], "json_class")
    expect_true(has_fields(ds_3[[i]], "retrievedConnections"))
    expect_identical(ds_3[[i]][["retrievedConnections"]],
                     list("PARENTS", "CHILDREN"))
  }
})

test_that("dataset references can be listed", {

  check_skip()

  mat <- material_id(c(3832L, 2475L), mode = "screening")

  ds_1 <- list_references(tok, mat[[1]], exp_ids[[1]])
  expect_identical(ds_1, list())

  ds_1 <- list_references(tok, mat[[2]], exp_ids[[1]])
  expect_is(ds_1, "PlateWellReferenceWithDatasets")
  expect_is(ds_1, "json_vec")
  expect_gte(length(ds_1), 1L)
  for (i in seq_along(ds_1)) {
    expect_is(ds_1[[i]], "PlateWellReferenceWithDatasets")
    expect_is(ds_1[[i]], "json_class")
    expect_true(has_fields(ds_1[[i]], "imageDatasetReferences"))
    for (j in seq_along(ds_1[[i]][["imageDatasetReferences"]])) {
      expect_is(ds_1[[i]][["imageDatasetReferences"]][[j]],
                "ImageDatasetReference")
      expect_is(ds_1[[i]][["imageDatasetReferences"]][[j]], "json_class")
    }
  }

  ds_2 <- list_references(tok, mat[1:2], exp_ids[[1]])
  expect_is(ds_2, "PlateWellReferenceWithDatasets")
  expect_is(ds_2, "json_vec")
  expect_gte(length(ds_2), length(ds_1))
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "PlateWellReferenceWithDatasets")
    expect_is(ds_2[[i]], "json_class")
    expect_true(has_fields(ds_2[[i]], "imageDatasetReferences"))
    for (j in seq_along(ds_2[[i]][["imageDatasetReferences"]])) {
      expect_is(ds_2[[i]][["imageDatasetReferences"]][[j]],
                "ImageDatasetReference")
      expect_is(ds_2[[i]][["imageDatasetReferences"]][[j]], "json_class")
    }
  }

  ds_1 <- list_references(tok, samples[[1]])
  expect_is(ds_1, "ImageDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_length(ds_1, 1L)
  expect_is(ds_1[[1L]], "ImageDatasetReference")
  expect_is(ds_1[[1L]], "json_class")

  ds_2 <- list_references(tok, samples[1:2])
  expect_is(ds_2, "ImageDatasetReference")
  expect_is(ds_2, "json_vec")
  expect_length(ds_2, 2L)
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "ImageDatasetReference")
    expect_is(ds_2[[i]], "json_class")
  }

  ds_1 <- list_references(tok, plates[[1]])
  expect_is(ds_1, "ImageDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_length(ds_1, 1L)
  expect_is(ds_1[[1L]], "ImageDatasetReference")
  expect_is(ds_1[[1L]], "json_class")

  ds_2 <- list_references(tok, plates[1:2])
  expect_is(ds_2, "ImageDatasetReference")
  expect_is(ds_2, "json_vec")
  expect_length(ds_2, 2L)
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "ImageDatasetReference")
    expect_is(ds_2[[i]], "json_class")
  }

  plate_ids <- as_plate_id(plates)

  expect_identical(list_references(tok, plate_ids[[1]]), ds_1)
  expect_identical(list_references(tok, plate_ids[1:2]), ds_2)

  expect_identical(list_references(tok, plate_meta[[1]]), ds_1)
  expect_identical(list_references(tok, plate_meta[1:2]), ds_2)

  ds_1 <- list_references(tok, plates[[1]], type = "feature")
  expect_is(ds_1, "FeatureVectorDatasetReference")
  expect_is(ds_1, "json_vec")
  expect_gte(length(ds_1), 1L)
  for (i in seq_along(ds_1)) {
    expect_is(ds_1[[i]], "FeatureVectorDatasetReference")
    expect_is(ds_1[[i]], "json_class")
  }

  dsids <- list_dataset_ids(tok, sapply(ds_2, `[[`, "datasetCode"))

  ds_1 <- list_references(tok, dsids[[1]], channels = "DAPI")
  expect_is(ds_1, "MicroscopyImageReference")
  expect_is(ds_1, "json_vec")
  expect_length(ds_1, 9L)
  for (i in seq_along(ds_1)) {
    expect_is(ds_1[[i]], "MicroscopyImageReference")
    expect_is(ds_1[[i]], "json_class")
  }

  ds_2 <- list_references(tok, dsids[1:2], channels = "DAPI")
  expect_is(ds_2, "MicroscopyImageReference")
  expect_is(ds_2, "json_vec")
  expect_length(ds_2, 18L)
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "MicroscopyImageReference")
    expect_is(ds_2[[i]], "json_class")
  }

  ds_3 <- list_references(tok, dsids[[1]], channels = c("DAPI", "GFP"))
  expect_is(ds_3, "MicroscopyImageReference")
  expect_is(ds_3, "json_vec")
  expect_length(ds_3, 18L)
  for (i in seq_along(ds_3)) {
    expect_is(ds_3[[i]], "MicroscopyImageReference")
    expect_is(ds_3[[i]], "json_class")
  }

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
  expect_length(ds_1, 9L)
  for (i in seq_along(ds_1)) {
    expect_is(ds_1[[i]], "PlateImageReference")
    expect_is(ds_1[[i]], "json_class")
  }

  ds_2 <- list_references(tok, dsids[[1]], wells = well_pos, channels = "DAPI")
  expect_is(ds_2, "PlateImageReference")
  expect_is(ds_2, "json_vec")
  expect_length(ds_2, 18L)
  for (i in seq_along(ds_2)) {
    expect_is(ds_2[[i]], "PlateImageReference")
    expect_is(ds_2[[i]], "json_class")
  }

  ds_3 <- list_references(tok, dsids[[1]], wells = well_pos[[1]],
                          channels = c("DAPI", "GFP"))
  expect_is(ds_3, "PlateImageReference")
  expect_is(ds_3, "json_vec")
  expect_length(ds_3, 18L)
  for (i in seq_along(ds_3)) {
    expect_is(ds_3[[i]], "PlateImageReference")
    expect_is(ds_3[[i]], "json_class")
  }
})

test_that("dataset types can be listed", {

  check_skip()

  ds_types <- list_dataset_types(tok)
  expect_is(ds_types, "DataSetType")
  expect_is(ds_types, "json_vec")
  expect_gte(length(ds_types), 1L)
  for (i in seq_along(ds_types)) {
    expect_is(ds_types[[i]], "DataSetType")
    expect_is(ds_types[[i]], "json_class")
  }
})

test_that("dataset ids can be listed", {

  check_skip()

  codes <- dataset_code(datasets)

  dsid_1 <- list_dataset_ids(tok, codes[[1]])
  expect_is(dsid_1, "DatasetIdentifier")
  expect_is(dsid_1, "json_vec")
  expect_length(dsid_1, 1L)
  expect_is(dsid_1[[1L]], "DatasetIdentifier")
  expect_is(dsid_1[[1L]], "json_class")

  dsid_2 <- list_dataset_ids(tok, codes[1:2])
  expect_is(dsid_2, "DatasetIdentifier")
  expect_is(dsid_2, "json_vec")
  expect_length(dsid_2, 2L)
  for (i in seq_along(dsid_2)) {
    expect_is(dsid_2[[i]], "DatasetIdentifier")
    expect_is(dsid_2[[i]], "json_class")
  }

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
