context("image")

test_that("experiment metadata can be listed", {
  meta_1 <- list_image_metadata(tok, exp_ids[[1]])
  expect_is(meta_1, "ExperimentImageMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_subclass(meta_1), "ExperimentImageMetadata")
  expect_true(all(sapply(meta_1, has_subclass,
                         "ExperimentImageMetadata")))
  expect_gte(length(meta_1), 1L)

  meta_2 <- list_image_metadata(tok, exp_ids[1:2])
  expect_is(meta_2, "ExperimentImageMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_subclass(meta_2), "ExperimentImageMetadata")
  expect_true(all(sapply(meta_2, has_subclass,
                         "ExperimentImageMetadata")))
  expect_gte(length(meta_2), length(meta_1))

  expect_identical(list_image_metadata(tok, experiments[[1]]), meta_1)
  expect_identical(list_image_metadata(tok, experiments[1:2]), meta_2)

  img_ds <- list_references(tok, plates[1:2])

  meta_1 <- list_image_metadata(tok, img_ds[[1]])
  expect_is(meta_1, "ImageDatasetMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_subclass(meta_1), "ImageDatasetMetadata")
  expect_true(all(sapply(meta_1, has_subclass,
                         "ImageDatasetMetadata")))
  expect_equal(length(meta_1), 1L)

  meta_2 <- list_image_metadata(tok, img_ds[1:2])
  expect_is(meta_2, "ImageDatasetMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_subclass(meta_2), "ImageDatasetMetadata")
  expect_true(all(sapply(meta_2, has_subclass,
                         "ImageDatasetMetadata")))
  expect_equal(length(meta_2), 2L)
})
