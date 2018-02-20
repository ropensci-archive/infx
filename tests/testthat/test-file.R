context("file")

test_that("files can be listed", {

  check_skip()

  codes <- sapply(datasets, `[[`, "code")

  files_1 <- list_files(tok, codes[2])
  expect_named(files_1)
  expect_is(files_1, "FileInfoDssDTO")
  expect_is(files_1, "json_vec")
  expect_identical(get_subclass(files_1), "FileInfoDssDTO")
  expect_true(all(sapply(files_1, has_subclass, "FileInfoDssDTO")))
  expect_gte(length(files_1), 1L)

  files_2 <- list_files(tok, codes[2:3])
  expect_named(files_2)
  expect_is(files_2, "FileInfoDssDTO")
  expect_is(files_2, "json_vec")
  expect_identical(get_subclass(files_2), "FileInfoDssDTO")
  expect_true(all(sapply(files_2, has_subclass, "FileInfoDssDTO")))
  expect_gte(length(files_2), length(files_1))

  expect_identical(list_files(tok, datasets[[2]]), files_1)
  expect_identical(list_files(tok, datasets[2:3]), files_2)

  ids <- list_dataset_ids(tok, codes[2:3])

  expect_identical(list_files(tok, ids[[1]]), files_1)
  expect_identical(list_files(tok, ids[1:2]), files_2)

  ds_file_1 <- json_class(dataSetCode = codes[2],
                          path = "",
                          isRecursive = TRUE,
                          class = "DataSetFileDTO")
  ds_file_2 <- c(ds_file_1,
                 json_class(dataSetCode = codes[3],
                            path = "",
                            isRecursive = TRUE,
                            class = "DataSetFileDTO"))

  expect_identical(list_files(tok, ds_file_1), files_1)
  expect_identical(list_files(tok, ds_file_2), files_2)
})

test_that("files can be fetched", {

  check_skip()

  codes <- sapply(datasets, `[[`, "code")

  files_1 <- list_files(tok, codes[2])
  is_file_1 <- !sapply(files_1, `[[`, "isDirectory")

  expect_warning(data <- fetch_files(tok, files_1, codes[2], n_con = 1L),
                 "cannot fetch directories")
  expect_length(data, sum(is_file_1))
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("data_set", "file", "data"))
    expect_is(data[[i]][["data_set"]], "character")
    expect_s3_class(data[[i]][["file"]], "FileInfoDssDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "raw")
  }

  expect_silent(fetch_files(tok, files_1[is_file_1], codes[2], n_con = 1L))
  expect_error(fetch_files(tok, files_1[is_file_1], codes[2], n_con = 1L,
                           n_try = 0L))

  files_2 <- list_files(tok, codes[2:3])
  is_file_2 <- !sapply(files_2, `[[`, "isDirectory")

  expect_error(data <- fetch_files(tok, files_2, codes[2:3], n_con = 1L))
  expect_warning(data <- fetch_files(tok, files_2, names(files_2), n_con = 1L),
                 "cannot fetch directories")
  expect_length(data, sum(is_file_2))
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("data_set", "file", "data"))
    expect_is(data[[i]][["data_set"]], "character")
    expect_s3_class(data[[i]][["file"]], "FileInfoDssDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "raw")
  }

  expect_silent(fetch_files(tok, files_2[is_file_2], names(files_2)[is_file_2],
                            n_con = 1L))

  files <- list_files(tok, "20120629084351794-603357")
  files <- files[grepl("Image\\.", sapply(files, `[[`, "pathInDataSet"))]

  expect_silent(data <- fetch_files(tok, files, "20120629084351794-603357",
                                    n_con = 5L))
  expect_length(data, length(files))
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("data_set", "file", "data"))
    expect_is(data[[i]][["data_set"]], "character")
    expect_s3_class(data[[i]][["file"]], "FileInfoDssDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "raw")
  }

  paths <- sapply(files_1[is_file_1], `[[`, "pathInDataSet")
  ds_file <- as_json_vec(lapply(paths, function(x) {
    json_class(dataSetCode = codes[2], path = x, isRecursive = FALSE,
               class = "DataSetFileDTO")
  }))

  data <- fetch_files(tok, ds_file, n_con = 1L)
  expect_length(data, length(ds_file))
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("file", "data"))
    expect_s3_class(data[[i]][["file"]], "DataSetFileDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "raw")
  }

  data <- fetch_files(tok, ds_file, n_con = 2L)
  expect_length(data, length(ds_file))
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("file", "data"))
    expect_s3_class(data[[i]][["file"]], "DataSetFileDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "raw")
  }

  expect_silent(fetch_files_serial("https://httpbin.org/get", n_try = 1L))
  expect_silent(fetch_files_serial(rep("https://httpbin.org/get", 2),
                                  n_try = 1L))
  expect_error(fetch_files_serial("https://httpbin.org/get", n_try = 1L,
                                  file_sizes = 10L))
  expect_error(fetch_files_serial("https://httpbin.org/status/500",
                                  n_try = 1L))
  expect_error(fetch_files_serial(rep("https://httpbin.org/get", 2),
                                  n_try = 1L, file_sizes = c(10L, 10L)))
  expect_error(fetch_files_serial(rep("https://httpbin.org/status/500", 2),
                                  n_try = 1L))

  expect_silent(fetch_files_parallel("https://httpbin.org/get", n_try = 1L))
  expect_silent(fetch_files_parallel(rep("https://httpbin.org/get", 2),
                                    n_try = 1L))
  expect_error(fetch_files_parallel("https://httpbin.org/get", n_try = 1L,
                                    file_sizes = 10L))
  expect_error(fetch_files_parallel("https://httpbin.org/status/500",
                                    n_try = 1L))
  expect_error(fetch_files_parallel(rep("https://httpbin.org/get", 2),
                                    n_try = 1L, file_sizes = c(10L, 10L)))
  expect_error(fetch_files_parallel(rep("https://httpbin.org/status/500", 2),
                                    n_try = 1L))
})