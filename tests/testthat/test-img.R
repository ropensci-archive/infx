context("image tools")

test_that("image datasets can be listed", {
  expect_is(img_ds <- list_img_datasets(tok, "KB03-2K"), "list")
  expect_true(all(sapply(img_ds, has_json_class, "ImageDatasetReference")))
  expect_gte(length(img_ds), 1L)
})
