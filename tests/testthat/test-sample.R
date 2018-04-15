context("sample")

test_that("samples can be listed", {

  samp_1 <- samples
  expect_s3_class(samp_1, "Sample")
  expect_s3_class(samp_1, "json_vec")
  expect_gte(length(samp_1), 1L)
  for (i in seq_along(samp_1)) {
    expect_s3_class(samp_1[[i]], "Sample")
    expect_s3_class(samp_1[[i]], "json_class")
    expect_identical(get_field(samp_1[[i]], "sampleTypeCode"), "PLATE")
  }

  check_skip()

  samp_2 <- list_samples(tok, exp_ids[c(1, 2)])
  expect_s3_class(samp_2, "Sample")
  expect_s3_class(samp_2, "json_vec")
  expect_gte(length(samp_2), 1L)
  for (i in seq_along(samp_2)) {
    expect_s3_class(samp_2[[i]], "Sample")
    expect_s3_class(samp_2[[i]], "json_class")
    expect_identical(get_field(samp_2[[i]], "sampleTypeCode"), "PLATE")
  }

  expect_identical(samp_1, list_samples(tok, experiments[[1]]))
  expect_identical(samp_2, list_samples(tok, experiments[1:2]))

  samp_1 <- list_samples(tok, plates[[1]])
  expect_s3_class(samp_1, "Sample")
  expect_s3_class(samp_1, "json_vec")
  expect_length(samp_1, 1L)
  expect_s3_class(samp_1[[1]], "Sample")
  expect_s3_class(samp_1[[1]], "json_class")
  expect_identical(get_field(samp_1[[1]], "sampleTypeCode"), "PLATE")

  samp_2 <- list_samples(tok, plates[c(1, 2)])
  expect_s3_class(samp_2, "Sample")
  expect_s3_class(samp_2, "json_vec")
  expect_length(samp_2, 2L)
  for (i in seq_along(samp_2)) {
    expect_s3_class(samp_2[[i]], "Sample")
    expect_s3_class(samp_2[[i]], "json_class")
    expect_identical(get_field(samp_2[[i]], "sampleTypeCode"), "PLATE")
  }

  samp_1 <- list_samples(tok, wells[[1]])
  expect_s3_class(samp_1, "Sample")
  expect_s3_class(samp_1, "json_vec")
  expect_length(samp_1, 1L)
  expect_s3_class(samp_1[[1]], "Sample")
  expect_s3_class(samp_1[[1]], "json_class")
  expect_false(get_field(samp_1[[1]], "sampleTypeCode") == "PLATE")

  samp_2 <- list_samples(tok, wells[c(1, 2)])
  expect_s3_class(samp_2, "Sample")
  expect_s3_class(samp_2, "json_vec")
  expect_length(samp_2, 2L)
  for (i in seq_along(samp_2)) {
    expect_s3_class(samp_2[[i]], "Sample")
    expect_s3_class(samp_2[[i]], "json_class")
  expect_false(get_field(samp_2[[i]], "sampleTypeCode") == "PLATE")
  }
})

test_that("sample types can be listed", {

  check_skip()

  samp_types <- list_sample_types(tok)
  expect_s3_class(samp_types, "SampleType")
  expect_s3_class(samp_types, "json_vec")
  expect_gte(length(samp_types), 1L)
  for (i in seq_along(samp_types)) {
    expect_s3_class(samp_types[[i]], "SampleType")
    expect_s3_class(samp_types[[i]], "json_class")
  }
})
