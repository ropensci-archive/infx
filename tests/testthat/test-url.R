context("urls can be determined")

test_that("data store servers can be listed", {
  dss <- list_datastores(tok)
  expect_is(dss, "DataStore")
  expect_is(dss, "json_vec")
  expect_identical(get_common_subclass(dss), "DataStore")
  expect_true(all(sapply(dss, has_json_subclass, "DataStore")))
  expect_gte(length(dss), 1L)
})

test_that("data store urls can be listed", {
  url <- list_datastore_urls(tok)
  expect_is(url, "character")
  expect_equal(length(url), 1L)

  exp_ids <- list_experiment_ids(tok)
  samples <- list_samples(tok, exp_ids[[1]])
  ds <- list_datasets(tok, samples[[1]])

  url_1 <- list_datastore_urls(tok, ds[[1]])
  expect_is(url_1, "character")
  expect_equal(length(url_1), 1L)

  url_2 <- list_datastore_urls(tok, ds[1:2])
  expect_is(url_2, "character")
  expect_equal(length(url_2), 2L)

  codes <- sapply(ds, `[[`, "code")

  url_1 <- list_datastore_urls(tok, codes[1])
  expect_is(url_1, "character")
  expect_equal(length(url_1), 1L)

  url_2 <- list_datastore_urls(tok, codes[1:2])
  expect_is(url_2, "character")
  expect_equal(length(url_2), 2L)

  dsids <- list_dataset_ids(tok, codes[1:2])
  url_1 <- list_datastore_urls(tok, dsids[[1]])
  expect_is(url_1, "character")
  expect_equal(length(url_1), 1L)

  url_2 <- list_datastore_urls(tok, dsids[1:2])
  expect_is(url_2, "character")
  expect_equal(length(url_2), 2L)
})
