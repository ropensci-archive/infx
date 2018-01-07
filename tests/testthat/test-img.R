context("image tools")

test_that("image datasets can be listed", {
  expect_s3_class(img_ds <- list_img_datasets(tok, "KB03-2K"),
                  c("ImageDatasetReference", "json_class"))
  expect_true(has_json_subclass(img_ds, "ImageDatasetReference"))
  expect_gte(length(img_ds), 1L)
  expect_is(segm_ds <- list_img_datasets(tok, "KB03-2K", "segmentation"),
            c("ImageDatasetReference", "json_class"))
  expect_true(has_json_subclass(segm_ds, "ImageDatasetReference"))
  expect_gte(length(segm_ds), 1L)
  expect_is(segm_ds <- list_img_datasets(tok, "KB03-2K", "segmentation",
                                         FALSE), "list")
  expect_true(all(sapply(segm_ds, has_json_subclass, "ImageDatasetReference")))
  expect_gte(length(segm_ds), 1L)
})
