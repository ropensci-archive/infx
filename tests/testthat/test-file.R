context("file")

test_that("files can be listed", {

  check_skip()

  codes <- dataset_code(datasets)

  files_1 <- list_files(tok, codes[2])
  expect_is(files_1, "FileInfoDssDTO")
  expect_is(files_1, "json_vec")
  expect_identical(get_subclass(files_1), "FileInfoDssDTO")
  expect_true(all(sapply(files_1, has_subclass, "FileInfoDssDTO")))
  expect_gte(length(files_1), 1L)

  files_2 <- list_files(tok, codes[2:3])
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

  codes <- dataset_code(datasets)

  files_1 <- list_files(tok, codes[2])
  is_file_1 <- !sapply(files_1, `[[`, "isDirectory")

  expect_warning(data <- fetch_files(tok, files_1, codes[2], n_con = 1L),
                 "cannot fetch directories")
  expect_length(data, sum(is_file_1))
  for (i in seq_along(data)) {
    expect_is(data[[i]], "raw")
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
  }

  data <- fetch_files(tok, files_1[is_file_1], n_con = 1L)
  expect_length(data, sum(is_file_1))
  for (i in seq_along(data)) {
    expect_is(data[[i]], "raw")
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
  }

  expect_error(fetch_files(tok, files_1[is_file_1], codes[2], n_con = 1L,
                           n_try = 0L))

  files_2 <- list_files(tok, codes[2:3])
  is_file_2 <- !sapply(files_2, `[[`, "isDirectory")

  expect_error(data <- fetch_files(tok, files_2, codes[2:3], n_con = 1L))
  expect_warning(data <- fetch_files(tok, files_2,
                                     sapply(files_2, attr, "data_set"),
                                     n_con = 1L),
                 "cannot fetch directories")
  expect_length(data, sum(is_file_2))
  for (i in seq_along(data)) {
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }

  expect_identical(fetch_files(tok, files_2[is_file_2],
                               sapply(files_2[is_file_2], attr, "data_set"),
                               n_con = 1L),
                   data)

  files <- list_files(tok, "20120629084351794-603357")
  files <- files[grepl("Image\\.", sapply(files, `[[`, "pathInDataSet"))]

  data <- fetch_files(tok, files, "20120629084351794-603357", n_con = 5L)
  expect_length(data, length(files))
  for (i in seq_along(data)) {
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }

  paths <- sapply(files_1[is_file_1], `[[`, "pathInDataSet")
  ds_file <- as_json_vec(lapply(paths, function(x) {
    json_class(dataSetCode = codes[2], path = x, isRecursive = FALSE,
               class = "DataSetFileDTO")
  }))

  data <- fetch_files(tok, ds_file, n_con = 1L)
  expect_length(data, length(ds_file))
  for (i in seq_along(data)) {
    expect_s3_class(attr(data[[i]], "file"), "DataSetFileDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }

  data <- fetch_files(tok, ds_file, n_con = 2L)
  expect_length(data, length(ds_file))
  for (i in seq_along(data)) {
    expect_s3_class(attr(data[[i]], "file"), "DataSetFileDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }

  data <- fetch_files(tok, codes[2])
  expect_gte(length(data), 1L)
  for (i in seq_along(data)) {
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }

  expect_identical(data, fetch_files(tok, codes[2], files_1[is_file_1]))
  expect_identical(data, fetch_files(tok, datasets[2], files_1[is_file_1]))

  files_chr <- sapply(files_1[is_file_1], `[[`, "pathInDataSet")

  expect_equivalent(data, fetch_files(tok, codes[2], files_chr, n_con = 1L))
  expect_equivalent(data, fetch_files(tok, codes[2], files_chr, n_con = 5L))

  data <- fetch_files(tok, codes[2], file_regex = "\\.std(out|err)$")
  expect_gte(length(data), 1L)
  for (i in seq_along(data)) {
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }
})

test_that("file fetchers work", {

  check_skip()

  dat <- do_requests_serial("https://httpbin.org/get", NA, n_try = 1L,
                            create_handle = infx:::create_file_handle,
                            check = infx:::check_file_result,
                            finally = identity)
  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_is(dat[[1]], "raw")

  dat <- do_requests_serial(rep("https://httpbin.org/get", 2), rep(NA, 2),
                            n_try = 1L,
                            create_handle = infx:::create_file_handle,
                            check = infx:::check_file_result,
                            finally = identity)
  expect_is(dat, "list")
  expect_length(dat, 2L)
  for (i in seq_along(dat))
    expect_is(dat[[i]], "raw")


  expect_warning(
    dat <- do_requests_serial("https://httpbin.org/get", 1000L, n_try = 1L,
                              create_handle = infx:::create_file_handle,
                              check = infx:::check_file_result,
                              finally = identity),
    "download incomplete"
  )

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_null(dat[[1L]])

  expect_warning(
    dat <- do_requests_serial("https://httpbin.org/status/500", NA, n_try = 1L,
                              create_handle = infx:::create_file_handle,
                              check = infx:::check_file_result,
                              finally = identity),
    "could not carry out request"
  )

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_null(dat[[1L]])

  expect_warning(
    dat <- do_requests_serial(rep("https://httpbin.org/get", 2), rep(1000L, 2),
                              n_try = 1L,
                              create_handle = infx:::create_file_handle,
                              check = infx:::check_file_result,
                              finally = identity),
    "download incomplete"
  )

  expect_is(dat, "list")
  expect_length(dat, 2L)
  for (i in seq_along(dat))
    expect_null(dat[[i]])

  expect_warning(
    dat <- do_requests_serial(rep("https://httpbin.org/status/500", 2),
                              rep(NA, 2),
                              n_try = 1L,
                              create_handle = infx:::create_file_handle,
                              check = infx:::check_file_result,
                              finally = identity),
    "could not carry out request"
  )

  expect_is(dat, "list")
  expect_length(dat, 2L)
  for (i in seq_along(dat))
    expect_null(dat[[i]])

  dat <- do_requests_parallel("https://httpbin.org/get", NA, n_try = 1L,
                              create_handle = infx:::create_file_handle,
                              check = infx:::check_file_result,
                              finally = identity)
  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_is(dat[[1]], "raw")

  dat <- do_requests_parallel(rep("https://httpbin.org/get", 2), rep(NA, 2),
                              n_try = 1L,
                              create_handle = infx:::create_file_handle,
                              check = infx:::check_file_result,
                              finally = identity)
  expect_is(dat, "list")
  expect_length(dat, 2L)
  for (i in seq_along(dat))
    expect_is(dat[[i]], "raw")

  expect_warning(
    dat <- do_requests_parallel("https://httpbin.org/get", 1000L, n_try = 1L,
                                create_handle = infx:::create_file_handle,
                                check = infx:::check_file_result,
                                finally = identity),
    "download incomplete"
  )

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_null(dat[[1L]])

  expect_warning(
    dat <- do_requests_parallel("https://httpbin.org/status/500", NA,
                                n_try = 1L,
                                create_handle = infx:::create_file_handle,
                                check = infx:::check_file_result,
                                finally = identity),
    "could not carry out request"
  )

  expect_is(dat, "list")
  expect_length(dat, 1L)
  expect_null(dat[[1L]])

  expect_warning(
    dat <- do_requests_parallel(rep("https://httpbin.org/get", 2),
                                rep(1000L, 2),
                                n_try = 1L,
                                create_handle = infx:::create_file_handle,
                                check = infx:::check_file_result,
                                finally = identity),
    "download incomplete"
  )

  expect_is(dat, "list")
  expect_length(dat, 2L)
  for (i in seq_along(dat))
    expect_null(dat[[i]])

  expect_warning(
    dat <- do_requests_parallel(rep("https://httpbin.org/status/500", 2),
                                rep(NA, 2),
                                n_try = 1L,
                                create_handle = infx:::create_file_handle,
                                check = infx:::check_file_result,
                                finally = identity),
    "could not carry out request"
  )

  expect_is(dat, "list")
  expect_length(dat, 2L)
  for (i in seq_along(dat))
    expect_null(dat[[i]])
})

test_that("matlab data files can be read", {

  data <- fetch_files(tok, "20120629084351794-603357",
                      file_regex = "Image\\.Count_[A-z]+\\.mat$",
                      reader = read_mat_files)

  expect_gte(length(data), 1L)
  for (i in seq_along(data)) {
    expect_is(data[[i]], "list")
    expect_true(all(sapply(data[[i]], is.numeric)))
    expect_true(assertthat::has_attr(data[[i]], "object"))
    expect_is(attr(data[[i]], "object"), "character")
    expect_true(assertthat::has_attr(data[[i]], "feature"))
    expect_is(attr(data[[i]], "feature"), "character")
    expect_true(assertthat::has_attr(data[[i]], "data_set"))
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_true(assertthat::has_attr(data[[i]], "file"))
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
  }

  expect_warning(data <- fetch_files(tok, "20120629084351794-603357",
                                     file_regex = "metadata.properties$",
                                     reader = read_mat_files))

  expect_gte(length(data), 1L)
  for (i in seq_along(data)) {
    expect_true(assertthat::has_attr(data[[i]], "data_set"))
    expect_is(attr(data[[i]], "data_set"), "character")
    expect_true(assertthat::has_attr(data[[i]], "file"))
    expect_s3_class(attr(data[[i]], "file"), "FileInfoDssDTO")
    expect_s3_class(attr(data[[i]], "file"), "json_class")
    expect_is(data[[i]], "raw")
  }
})
