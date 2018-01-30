context("image")

test_that("experiment metadata can be listed", {
  exp_ids <- list_experiment_ids(tok)

  meta_1 <- list_image_metadata(tok, exp_ids[[1]])
  expect_is(meta_1, "ExperimentImageMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_common_subclass(meta_1), "ExperimentImageMetadata")
  expect_true(all(sapply(meta_1, has_json_subclass,
                         "ExperimentImageMetadata")))
  expect_gte(length(meta_1), 1L)

  meta_2 <- list_image_metadata(tok, exp_ids[1:2])
  expect_is(meta_2, "ExperimentImageMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_common_subclass(meta_2), "ExperimentImageMetadata")
  expect_true(all(sapply(meta_2, has_json_subclass,
                         "ExperimentImageMetadata")))
  expect_gte(length(meta_2), length(meta_1))

  exps <- list_experiments(tok, exp_ids[1:2])
  expect_identical(list_image_metadata(tok, exps[[1]]), meta_1)
  expect_identical(list_image_metadata(tok, exps[1:2]), meta_2)
})
