context("sample")

test_that("samples can be listed", {

  samp_1 <- samples
  expect_is(samp_1, "Sample")
  expect_is(samp_1, "json_vec")
  expect_identical(get_subclass(samp_1), "Sample")
  expect_true(all(sapply(samp_1, has_subclass, "Sample")))
  expect_gte(length(samp_1), 1L)

  check_skip()

  samp_2 <- list_samples(tok, exp_ids[c(1, 2)])
  expect_is(samp_2, "Sample")
  expect_is(samp_2, "json_vec")
  expect_identical(get_subclass(samp_2), "Sample")
  expect_true(all(sapply(samp_2, has_subclass, "Sample")))
  expect_gte(length(samp_2), 1L)

  expect_identical(samp_1, list_samples(tok, experiments[[1]]))
  expect_identical(samp_2, list_samples(tok, experiments[1:2]))

  samp_1 <- list_samples(tok, plates[[1]])
  expect_is(samp_1, "Sample")
  expect_is(samp_1, "json_vec")
  expect_identical(get_subclass(samp_1), "Sample")
  expect_true(all(sapply(samp_1, has_subclass, "Sample")))
  expect_equal(length(samp_1), 1L)

  samp_2 <- list_samples(tok, plates[c(1, 2)])
  expect_is(samp_2, "Sample")
  expect_is(samp_2, "json_vec")
  expect_identical(get_subclass(samp_2), "Sample")
  expect_true(all(sapply(samp_2, has_subclass, "Sample")))
  expect_equal(length(samp_2), 2L)

  samp_1 <- list_samples(tok, wells[[1]])
  expect_is(samp_1, "Sample")
  expect_is(samp_1, "json_vec")
  expect_identical(get_subclass(samp_1), "Sample")
  expect_true(all(sapply(samp_1, has_subclass, "Sample")))
  expect_equal(length(samp_1), 1L)

  samp_2 <- list_samples(tok, wells[c(1, 2)])
  expect_is(samp_2, "Sample")
  expect_is(samp_2, "json_vec")
  expect_identical(get_subclass(samp_2), "Sample")
  expect_true(all(sapply(samp_2, has_subclass, "Sample")))
  expect_equal(length(samp_2), 2L)
})

test_that("sample types can be listed", {

  check_skip()

  samp_types <- list_sample_types(tok)
  expect_is(samp_types, "SampleType")
  expect_is(samp_types, "json_vec")
  expect_identical(get_subclass(samp_types), "SampleType")
  expect_true(all(sapply(samp_types, has_subclass, "SampleType")))
  expect_gte(length(samp_types), 1L)
})
