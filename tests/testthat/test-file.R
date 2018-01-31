context("file")

test_that("files can be listed", {
  exp_ids <- list_experiment_ids(tok)
  samples <- list_samples(tok, exp_ids[[1]])
  ds <- list_datasets(tok, samples[[1]])
  codes <- sapply(ds, `[[`, "code")

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

  expect_identical(list_files(tok, ds[[2]]), files_1)
  expect_identical(list_files(tok, ds[2:3]), files_2)

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
