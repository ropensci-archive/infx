context("urls can be determined")

test_that("data store servers can be listed", {

  check_skip()

  dss <- list_datastores(tok)
  expect_is(dss, "DataStore")
  expect_is(dss, "json_vec")
  expect_identical(get_subclass(dss), "DataStore")
  expect_true(all(sapply(dss, has_subclass, "DataStore")))
  expect_gte(length(dss), 1L)
})

test_that("data store urls can be listed", {

  check_skip()

  url <- list_datastore_urls(tok)
  expect_is(url, "character")
  expect_equal(length(url), 1L)

  url_1 <- list_datastore_urls(tok, datasets[[1]])
  expect_is(url_1, "character")
  expect_equal(length(url_1), 1L)

  url_2 <- list_datastore_urls(tok, datasets[1:2])
  expect_is(url_2, "character")
  expect_equal(length(url_2), 2L)

  codes <- sapply(datasets, `[[`, "code")

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

test_that("dataset download urls can be generated", {

  check_skip()

  codes <- sapply(datasets, `[[`, "code")

  ds_file <- json_class(dataSetCode = codes[2],
                        path = "",
                        isRecursive = TRUE,
                        class = "DataSetFileDTO")
  files <- Filter(function(x) !x[["isDirectory"]], list_files(tok, ds_file))
  paths <- sapply(files, `[[`, "pathInDataSet")

  url_1 <- list_download_urls(tok, codes[2], paths[1])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)
  expect_true(grepl("^https://", url_1))

  url_2 <- list_download_urls(tok, codes[2], paths[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
  expect_true(all(grepl("^https://", url_2)))

  url_to <- list_download_urls(tok, codes[2], paths[1], 5L)
  expect_is(url_to, "character")
  expect_length(url_to, 1L)
  expect_true(grepl("^https://", url_to))

  url_1 <- list_download_urls(tok, datasets[[2]], paths[1])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)
  expect_true(grepl("^https://", url_1))

  url_2 <- list_download_urls(tok, datasets[[2]], paths[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
  expect_true(all(grepl("^https://", url_2)))

  dsid <- list_dataset_ids(tok, codes[2])

  url_1 <- list_download_urls(tok, dsid[[1]], paths[1])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)
  expect_true(grepl("^https://", url_1))

  url_2 <- list_download_urls(tok, dsid[[1]], paths[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
  expect_true(all(grepl("^https://", url_2)))

  ds_file <- c(json_class(dataSetCode = codes[2],
                          path = paths[1],
                          isRecursive = FALSE,
                          class = "DataSetFileDTO"),
               json_class(dataSetCode = codes[2],
                          path = paths[2],
                          isRecursive = FALSE,
                          class = "DataSetFileDTO"))

  url_1 <- list_download_urls(tok, ds_file[[1]])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)
  expect_true(grepl("^https://", url_1))

  url_2 <- list_download_urls(tok, ds_file[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
  expect_true(all(grepl("^https://", url_2)))

  url_to <- list_download_urls(tok, ds_file[[1]], 5L)
  expect_is(url_to, "character")
  expect_length(url_to, 1L)
  expect_true(grepl("^https://", url_to))
})
