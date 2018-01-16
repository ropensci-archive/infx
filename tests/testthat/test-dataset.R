context("dataset")

test_that("dataset types can be listed", {
  ds_types <- list_dataset_types(tok)
  expect_is(ds_types, "DataSetType")
  expect_is(ds_types, "json_vec")
  expect_identical(get_common_subclass(ds_types), "DataSetType")
})
