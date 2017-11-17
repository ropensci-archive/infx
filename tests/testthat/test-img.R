context("image tools")

test_that("image datasets can be listed", {
  expect_is(img_ds <- list_img_datasets(tok, "KB03-2K"), "list")
  expect_true(has_json_class(img_ds, "ImageDatasetReference"))
  expect_gte(length(img_ds), 1L)
  expect_is(segm_ds <- list_img_datasets(tok, "KB03-2K", "segmentation"),
            "list")
  expect_true(has_json_class(segm_ds, "ImageDatasetReference"))
  expect_gte(length(segm_ds), 1L)
  expect_is(segm_ds <- list_img_datasets(tok, "KB03-2K", "segmentation",
                                         FALSE), "list")
  expect_true(all(sapply(segm_ds, has_json_class, "ImageDatasetReference")))
  expect_gte(length(segm_ds), 1L)
})
