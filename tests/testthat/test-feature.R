context("list features")

test_that("features and feature codes can be listed", {
  exp_ids <- list_experiment_ids(tok)
  plates <- list_plates(tok, exp_ids[[1]])
  refs <- list_references(tok, plates[[1]], type = "feature")

  feat_1 <- list_features(tok, refs[[1]])
  expect_is(feat_1, "FeatureInformation")
  expect_is(feat_1, "json_vec")
  expect_identical(get_subclass(feat_1), "FeatureInformation")
  expect_true(all(sapply(feat_1, has_subclass, "FeatureInformation")))
  expect_gte(length(feat_1), 1L)

  feat_2 <- list_features(tok, refs[1:2])
  expect_is(feat_2, "FeatureInformation")
  expect_is(feat_2, "json_vec")
  expect_identical(get_subclass(feat_2), "FeatureInformation")
  expect_true(all(sapply(feat_2, has_subclass, "FeatureInformation")))
  expect_gte(length(feat_2), length(feat_1))

  expect_identical(list_feature_codes(tok, refs[[1]]),
                   lapply(feat_1, `[[`, "code"))
  expect_identical(list_feature_codes(tok, refs[1:2]),
                   lapply(feat_2, `[[`, "code"))
})
