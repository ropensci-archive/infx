context("test readers")

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
