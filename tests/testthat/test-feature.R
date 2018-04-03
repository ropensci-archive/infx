context("list features")

test_that("features and feature codes can be listed", {

  check_skip()

  refs <- list_references(tok, plates[[1]], type = "feature")
  wells <- json_vec(
    json_class(wellRow = 1L, wellColumn = 1L, class = "WellPosition"),
    json_class(wellRow = 1L, wellColumn = 2L, class = "WellPosition")
  )

  feat_1 <- list_features(tok, refs[[1]])
  expect_is(feat_1, "FeatureInformation")
  expect_is(feat_1, "json_vec")
  expect_identical(get_subclass(feat_1), "FeatureInformation")
  expect_gte(length(feat_1), 1L)
  for (i in seq_along(feat_1))
    expect_true(has_subclass(feat_1[[i]], "FeatureInformation"))

  feat_2 <- list_features(tok, refs[1:2])
  expect_is(feat_2, "FeatureInformation")
  expect_is(feat_2, "json_vec")
  expect_identical(get_subclass(feat_2), "FeatureInformation")
  expect_gte(length(feat_2), length(feat_1))
  for (i in seq_along(feat_2))
    expect_true(has_subclass(feat_2[[i]], "FeatureInformation"))

  expect_identical(list_features(tok, refs[[1]], wells[[1]]),
                   feat_1)
  expect_identical(list_features(tok, refs[[1]], wells),
                   feat_1)
  expect_identical(list_features(tok, refs[1:2], wells[[1]]),
                   feat_2)
  expect_identical(list_features(tok, refs[1:2], wells),
                   feat_2)

  expect_identical(list_feature_codes(tok, refs[[1]]),
                   lapply(feat_1, `[[`, "code"))
  expect_identical(list_feature_codes(tok, refs[1:2]),
                   lapply(feat_2, `[[`, "code"))
})

test_that("features can be fetched", {

  check_skip()

  refs <- list_references(tok, plates[[1]], type = "feature")
  feats <- list_features(tok, refs[[1]])

  codes <- sapply(feats, `[[`, "code")

  feat_data_11 <- fetch_features(tok, refs[[1]], codes[1])
  expect_is(feat_data_11, "FeatureVectorDataset")
  expect_is(feat_data_11, "json_vec")
  expect_identical(get_subclass(feat_data_11), "FeatureVectorDataset")
  expect_true(has_fields(feat_data_11, "featureVectors"))
  expect_length(feat_data_11, 1L)
  for (i in seq_along(feat_data_11)) {
    expect_true(has_subclass(feat_data_11[[i]], "FeatureVectorDataset"))
    expect_length(feat_data_11[[i]][["featureVectors"]], 384L)
    for (j in seq_along(feat_data_11[[i]])) {
      expect_true(has_subclass(feat_data_11[[i]][["featureVectors"]][[j]],
                               "FeatureVector"))
      expect_true(has_fields(feat_data_11[[i]][["featureVectors"]][[j]],
                             "values"))
      expect_length(feat_data_11[[i]][["featureVectors"]][[j]][["values"]], 1L)
    }
  }

  feat_data_12 <- fetch_features(tok, refs[[1]], codes[1:2])
  expect_is(feat_data_12, "FeatureVectorDataset")
  expect_is(feat_data_12, "json_vec")
  expect_identical(get_subclass(feat_data_12), "FeatureVectorDataset")
  expect_true(has_fields(feat_data_12, "featureVectors"))
  expect_length(feat_data_12, 1L)
  for (i in seq_along(feat_data_12)) {
    expect_true(has_subclass(feat_data_12[[i]], "FeatureVectorDataset"))
    expect_length(feat_data_12[[i]][["featureVectors"]], 384L)
    for (j in seq_along(feat_data_12[[i]])) {
      expect_true(has_subclass(feat_data_12[[i]][["featureVectors"]][[j]],
                               "FeatureVector"))
      expect_true(has_fields(feat_data_12[[i]][["featureVectors"]][[j]],
                             "values"))
      expect_length(feat_data_12[[i]][["featureVectors"]][[j]][["values"]], 2L)
    }
  }

  feat_data_1 <- fetch_features(tok, refs[[1]])
  expect_is(feat_data_1, "FeatureVectorDataset")
  expect_is(feat_data_1, "json_vec")
  expect_identical(get_subclass(feat_data_1), "FeatureVectorDataset")
  expect_true(has_fields(feat_data_1, "featureVectors"))
  expect_length(feat_data_1, 1L)
  for (i in seq_along(feat_data_1)) {
    expect_true(has_subclass(feat_data_1[[i]], "FeatureVectorDataset"))
    expect_length(feat_data_1[[i]][["featureVectors"]], 384L)
    for (j in seq_along(feat_data_1[[i]])) {
      expect_true(has_subclass(feat_data_1[[i]][["featureVectors"]][[j]],
                               "FeatureVector"))
      expect_true(has_fields(feat_data_1[[i]][["featureVectors"]][[j]],
                             "values"))
      expect_length(feat_data_1[[i]][["featureVectors"]][[j]][["values"]],
                    length(codes))
    }
  }

  well_pos <- lapply(feat_data_11[[1]][["featureVectors"]], `[[`,
                     "wellPosition")
  refs_well <- json_vec(
    json_class(datasetCode = refs[[1]][["datasetCode"]],
               datastoreServerUrl = refs[[1]][["datastoreServerUrl"]],
               plate = refs[[1]][["plate"]],
               experimentIdentifier = refs[[1]][["experimentIdentifier"]],
               plateGeometry = refs[[1]][["plateGeometry"]],
               registrationDate = refs[[1]][["registrationDate"]],
               properties = refs[[1]][["properties"]],
               wellPosition = well_pos[[1]],
               class = "FeatureVectorDatasetWellReference"),
    json_class(datasetCode = refs[[1]][["datasetCode"]],
               datastoreServerUrl = refs[[1]][["datastoreServerUrl"]],
               plate = refs[[1]][["plate"]],
               experimentIdentifier = refs[[1]][["experimentIdentifier"]],
               plateGeometry = refs[[1]][["plateGeometry"]],
               registrationDate = refs[[1]][["registrationDate"]],
               properties = refs[[1]][["properties"]],
               wellPosition = well_pos[[2]],
               class = "FeatureVectorDatasetWellReference"))

  feat_data_11 <- fetch_features(tok, refs_well[[1]], codes[1])
  expect_is(feat_data_11, "FeatureVectorWithDescription")
  expect_is(feat_data_11, "json_vec")
  expect_identical(get_subclass(feat_data_11), "FeatureVectorWithDescription")
  expect_length(feat_data_11, 1L)
  expect_true(has_subclass(feat_data_11[[1]], "FeatureVectorWithDescription"))
  expect_true(has_fields(feat_data_11[[1]], "values"))
  expect_length(feat_data_11[[1]][["values"]], 1L)

  expect_identical(fetch_features(tok, refs[[1]], codes[1], well_pos[[1]]),
                   feat_data_11)

  feat_data_12 <- fetch_features(tok, refs_well[[1]], codes[1:2])
  expect_is(feat_data_12, "FeatureVectorWithDescription")
  expect_is(feat_data_12, "json_vec")
  expect_identical(get_subclass(feat_data_12), "FeatureVectorWithDescription")
  expect_length(feat_data_12, 1L)
  expect_true(has_subclass(feat_data_12[[1]], "FeatureVectorWithDescription"))
  expect_true(has_fields(feat_data_12[[1]], "values"))
  expect_length(feat_data_12[[1]][["values"]], 2L)

  expect_identical(fetch_features(tok, refs[[1]], codes[1:2], well_pos[[1]]),
                   feat_data_12)

  feat_data_1 <- fetch_features(tok, refs_well[[1]])
  expect_is(feat_data_1, "FeatureVectorWithDescription")
  expect_is(feat_data_1, "json_vec")
  expect_identical(get_subclass(feat_data_1), "FeatureVectorWithDescription")
  expect_length(feat_data_1, 1L)
  expect_true(has_subclass(feat_data_1[[1]], "FeatureVectorWithDescription"))
  expect_true(has_fields(feat_data_1[[1]], "values"))
  expect_length(feat_data_1[[1]][["values"]], length(codes))

  expect_identical(fetch_features(tok, refs[[1]], wells = well_pos[[1]]),
                   feat_data_1)

  feat_data_21 <- fetch_features(tok, refs_well, codes[1])
  expect_is(feat_data_21, "FeatureVectorWithDescription")
  expect_is(feat_data_21, "json_vec")
  expect_identical(get_subclass(feat_data_21), "FeatureVectorWithDescription")
  expect_length(feat_data_21, 2L)
  for (i in seq_along(feat_data_21)) {
    expect_true(has_subclass(feat_data_21[[i]],
                             "FeatureVectorWithDescription"))
    expect_true(has_fields(feat_data_21[[i]], "values"))
    expect_length(feat_data_21[[i]][["values"]], 1L)
  }

  expect_identical(fetch_features(tok, refs[[1]], codes[1], well_pos[1:2]),
                   feat_data_21)

  feat_data_22 <- fetch_features(tok, refs_well, codes[1:2])
  expect_is(feat_data_22, "FeatureVectorWithDescription")
  expect_is(feat_data_22, "json_vec")
  expect_identical(get_subclass(feat_data_22), "FeatureVectorWithDescription")
  expect_length(feat_data_22, 2L)
  for (i in seq_along(feat_data_22)) {
    expect_true(has_subclass(feat_data_22[[i]],
                             "FeatureVectorWithDescription"))
    expect_true(has_fields(feat_data_22[[i]], "values"))
    expect_length(feat_data_22[[i]][["values"]], 2L)
  }

  expect_identical(fetch_features(tok, refs[[1]], codes[1:2], well_pos[1:2]),
                   feat_data_22)

  feat_data_12 <- fetch_features(tok, refs_well[[1]], codes[1:2])
  expect_is(feat_data_12, "FeatureVectorWithDescription")
  expect_is(feat_data_12, "json_vec")
  expect_identical(get_subclass(feat_data_12), "FeatureVectorWithDescription")
  expect_length(feat_data_12, 1L)
  expect_true(has_subclass(feat_data_12[[1]], "FeatureVectorWithDescription"))
  expect_true(has_fields(feat_data_12[[1]], "values"))
  expect_length(feat_data_12[[1]][["values"]], 2L)

  expect_identical(fetch_features(tok, refs[[1]], codes[1:2], well_pos[[1]]),
                   feat_data_12)
})

test_that("feature dataset refs can be converted", {

  check_skip()

  refs <- list_references(tok, plates[[1]], type = "feature")
  wells <- json_vec(
    json_class(wellRow = 1L, wellColumn = 1L, class = "WellPosition"),
    json_class(wellRow = 1L, wellColumn = 2L, class = "WellPosition")
  )

  fields <- c("datasetCode", "datastoreServerUrl", "plate",
              "experimentIdentifier", "plateGeometry", "registrationDate",
              "properties", "wellPosition")

  well_ref_11 <- feat_ds_well_ref(refs[[1]], wells[[1]])
  expect_is(well_ref_11, "FeatureVectorDatasetWellReference")
  expect_is(well_ref_11, "json_vec")
  expect_true(has_subclass(well_ref_11, "FeatureVectorDatasetWellReference"))
  expect_length(well_ref_11, 1L)
  expect_true(has_subclass(well_ref_11[[1]],
                           "FeatureVectorDatasetWellReference"))
  expect_true(has_fields(well_ref_11[[1]], fields))
  expect_true(has_subclass(well_ref_11[[1]][["plate"]],
                           "PlateIdentifier"))
  expect_true(has_subclass(well_ref_11[[1]][["experimentIdentifier"]],
                           "ExperimentIdentifier"))
  expect_true(has_subclass(well_ref_11[[1]][["plateGeometry"]],
                           "Geometry"))
  expect_true(has_subclass(well_ref_11[[1]][["wellPosition"]],
                           "WellPosition"))

  well_ref_12 <- feat_ds_well_ref(refs[[1]], wells)
  expect_is(well_ref_12, "FeatureVectorDatasetWellReference")
  expect_is(well_ref_12, "json_vec")
  expect_true(has_subclass(well_ref_12, "FeatureVectorDatasetWellReference"))
  expect_length(well_ref_12, 2L)
  for (i in seq_along(well_ref_12)) {
    expect_true(has_subclass(well_ref_12[[i]],
                             "FeatureVectorDatasetWellReference"))
    expect_true(has_fields(well_ref_12[[i]], fields))
    expect_true(has_subclass(well_ref_12[[i]][["plate"]],
                             "PlateIdentifier"))
    expect_true(has_subclass(well_ref_12[[i]][["experimentIdentifier"]],
                             "ExperimentIdentifier"))
    expect_true(has_subclass(well_ref_12[[i]][["plateGeometry"]],
                             "Geometry"))
    expect_true(has_subclass(well_ref_12[[i]][["wellPosition"]],
                             "WellPosition"))
  }

  well_ref_21 <- feat_ds_well_ref(refs[1:2], wells[[1]])
  expect_is(well_ref_21, "FeatureVectorDatasetWellReference")
  expect_is(well_ref_21, "json_vec")
  expect_true(has_subclass(well_ref_21, "FeatureVectorDatasetWellReference"))
  expect_length(well_ref_21, 2L)
  for (i in seq_along(well_ref_21)) {
    expect_true(has_subclass(well_ref_21[[i]],
                             "FeatureVectorDatasetWellReference"))
    expect_true(has_fields(well_ref_21[[i]], fields))
    expect_true(has_subclass(well_ref_21[[i]][["plate"]],
                             "PlateIdentifier"))
    expect_true(has_subclass(well_ref_21[[i]][["experimentIdentifier"]],
                             "ExperimentIdentifier"))
    expect_true(has_subclass(well_ref_21[[i]][["plateGeometry"]],
                             "Geometry"))
    expect_true(has_subclass(well_ref_21[[i]][["wellPosition"]],
                             "WellPosition"))
  }

  well_ref_22 <- feat_ds_well_ref(refs[1:2], wells)
  expect_is(well_ref_22, "FeatureVectorDatasetWellReference")
  expect_is(well_ref_22, "json_vec")
  expect_true(has_subclass(well_ref_22, "FeatureVectorDatasetWellReference"))
  expect_length(well_ref_22, 2L)
  for (i in seq_along(well_ref_22)) {
    expect_true(has_subclass(well_ref_22[[i]],
                             "FeatureVectorDatasetWellReference"))
    expect_true(has_fields(well_ref_22[[i]], fields))
    expect_true(has_subclass(well_ref_22[[i]][["plate"]],
                             "PlateIdentifier"))
    expect_true(has_subclass(well_ref_22[[i]][["experimentIdentifier"]],
                             "ExperimentIdentifier"))
    expect_true(has_subclass(well_ref_22[[i]][["plateGeometry"]],
                             "Geometry"))
    expect_true(has_subclass(well_ref_22[[i]][["wellPosition"]],
                             "WellPosition"))
  }

  expect_error(feat_ds_well_ref(refs, wells))
})
