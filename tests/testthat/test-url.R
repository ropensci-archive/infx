context("urls can be determined")

test_that("data store servers can be listed", {

  check_skip()

  dss <- list_datastores(tok)
  expect_s3_class(dss, "DataStore")
  expect_s3_class(dss, "json_vec")
  expect_identical(get_subclass(dss), "DataStore")
  expect_length(dss, 1L)
  expect_true(has_subclass(dss, "DataStore"))
})

test_that("data store urls can be listed", {

  check_skip()

  url <- list_datastore_urls(tok)
  expect_is(url, "character")
  expect_length(url, 1L)

  url_1 <- list_datastore_urls(tok, datasets[[1]])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)

  url_2 <- list_datastore_urls(tok, datasets[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)

  codes <- dataset_code(datasets)

  url_1 <- list_datastore_urls(tok, codes[1])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)

  url_2 <- list_datastore_urls(tok, codes[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)

  dsids <- list_dataset_ids(tok, codes[1:2])
  url_1 <- list_datastore_urls(tok, dsids[[1]])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)

  url_2 <- list_datastore_urls(tok, dsids[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
})

test_that("dataset download urls can be generated", {

  check_skip()

  codes <- dataset_code(datasets)

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
  for (i in seq_along(url_2))
    expect_match(url_2[i], "^https://")

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
  for (i in seq_along(url_2))
    expect_match(url_2[i], "^https://")

  url_2 <- list_download_urls(tok,
                              c("20150518113941960-3132048",
                                "20160421133225062-3373964"),
                              "original/data/metadata.properties")
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
  for (i in seq_along(url_2))
    expect_match(url_2[i], "^https://")

  dsid <- list_dataset_ids(tok, codes[2])

  url_1 <- list_download_urls(tok, dsid[[1]], paths[1])
  expect_is(url_1, "character")
  expect_length(url_1, 1L)
  expect_true(grepl("^https://", url_1))

  url_2 <- list_download_urls(tok, dsid[[1]], paths[1:2])
  expect_is(url_2, "character")
  expect_length(url_2, 2L)
  for (i in seq_along(url_2))
    expect_match(url_2[i], "^https://")

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
  for (i in seq_along(url_2))
    expect_match(url_2[i], "^https://")

  url_to <- list_download_urls(tok, ds_file[[1]], 5L)
  expect_is(url_to, "character")
  expect_length(url_to, 1L)
  expect_true(grepl("^https://", url_to))
})

test_that("openbis api urls and docs links can be generated", {
  url <- api_url()
  expect_is(url, "character")
  expect_length(url, 1L)
  expect_match(url, "^https://")
  expect_identical(url, api_url("gis"))

  link <- docs_link()
  expect_is(link, "character")
  expect_length(link, 1L)
  expect_match(link, "^\\\\href\\{.+\\}\\{.+\\}$")
  expect_identical(link, docs_link("gis"))

  link <- docs_link(method_name = "foo")
  expect_is(link, "character")
  expect_length(link, 1L)
  expect_match(link, "^\\\\href\\{.+\\}\\{.+\\:foo}$")
  expect_identical(link, docs_link("gis", method_name = "foo"))
})

test_that("non-infectx openbis instances can be accessed", {

  check_skip()

  token <- login_openbis("test_observer", "test_observer",
                         auto_disconnect = FALSE,
                         host_url = "https://openbis.elnlims.ch")

  expect_is(token, "character")
  expect_length(token, 1L)
  expect_match(token, "^test_observer-")

  expect_true(is_token_valid(token, host_url = "https://openbis.elnlims.ch"))

  proj <- list_projects(token, host_url = "https://openbis.elnlims.ch")

  expect_s3_class(proj, "Project")
  expect_s3_class(proj, "json_vec")
  expect_gte(length(proj), 1L)
  for (i in seq_along(proj)) {
    expect_s3_class(proj[[i]], "Project")
    expect_s3_class(proj[[i]], "json_class")
  }

  flow <- search_openbis(
    token,
    search_criteria(
      property_clause("name", "Flow citometry files"),
      sub_criteria = search_sub_criteria(
        search_criteria(attribute_clause(value = "INDUCTION_OF_TF")),
        type = "experiment"
      )
    ),
    host_url = "https://openbis.elnlims.ch"
  )

  files <- fetch_files(token, flow,
                       file_regex = "11\\.fcs$",
                       host_url = "https://openbis.elnlims.ch")

  expect_gte(length(files), 1L)
  for (i in seq_along(files)) {
    expect_s3_class(attr(files[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(files[[i]], "file"), "json_class")
    expect_is(files[[i]], "raw")
  }

  expect_null(logout_openbis(token, host_url = "https://openbis.elnlims.ch"))
  expect_false(is_token_valid(token, host_url = "https://openbis.elnlims.ch"))
})
