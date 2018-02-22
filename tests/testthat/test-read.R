context("test readers")

test_that("matlab data files can be read", {

  data <- fetch_files(tok, "20120629084351794-603357",
                      file_regex = "Image\\.Count_[A-z]+\\.mat$",
                      done = read_mat_files)

  expect_gte(length(data), 1L)
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("data_set", "file", "data"))
    expect_is(data[[i]][["data_set"]], "character")
    expect_s3_class(data[[i]][["file"]], "FileInfoDssDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "list")
    expect_true(all(sapply(data[[i]][["data"]], is.numeric)))
    expect_true(assertthat::has_attr(data[[i]][["data"]], "object"))
    expect_true(assertthat::has_attr(data[[i]][["data"]], "feature"))
  }

  expect_warning(data <- fetch_files(tok, "20120629084351794-603357",
                                     file_regex = "metadata.properties$",
                                     done = read_mat_files))

  expect_gte(length(data), 1L)
  for (i in seq_along(data)) {
    expect_named(data[[i]], c("data_set", "file", "data"))
    expect_is(data[[i]][["data_set"]], "character")
    expect_s3_class(data[[i]][["file"]], "FileInfoDssDTO")
    expect_s3_class(data[[i]][["file"]], "json_class")
    expect_is(data[[i]][["data"]], "raw")
  }
})
