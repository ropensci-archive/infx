context("sample")

test_that("sample types can be listed", {
  samp_types <- list_sample_types(tok)
  expect_is(samp_types, "SampleType")
  expect_is(samp_types, "json_vec")
  expect_identical(get_common_subclass(samp_types), "SampleType")
})
