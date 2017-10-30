context("openbis downloader")

test_that("openbis system call works", {
  expect_error(fetch_openbis())
  dir <- tempfile()
  expect_true(dir.create(dir))
  tmp <- fetch_openbis(data_id = "20150421161324410-3129549", out_dir = dir)
  expect_match(tmp, "^Successfully downloaded 2 files", all = FALSE)
  tmp <- fetch_openbis(plate_regex = "^/.*/BB02-2E$",
                       data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                       file_regex = ".*Children_[A-z]+_Count.mat$",
                       out_dir = dir)
  expect_match(tmp, "does not exist, will download.$", all = FALSE)
  unlink(dir, recursive = TRUE)
})

test_that("data can be fetched", {
  dir <- tempfile()
  expect_true(dir.create(dir))
  expect_error(fetch_data(plate_regex = "^/.*/BB02-2E$",
                          data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                          file_regex = ".*Children_[A-z]+_Count.mat$",
                          out_dir = dir))
  unlink(dir, recursive = TRUE)
  expect_warning(fetch_data(plate_regex = "^/.*/BB02-2E$",
                            data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                            file_regex = ".*Children_[A-z]+_Count.mat$",
                            out_dir = dir, verbosity = 12))
  unlink(dir, recursive = TRUE)
  dat <- fetch_data(plate_regex = "^/.*/BB02-2E$",
                    data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                    file_regex = ".*Children_[A-z]+_Count.mat$",
                    out_dir = dir)
  expect_length(dat, 4L)
  expect_match(dat, ".*Children_[A-z]+_Count.mat$", all = TRUE)
  expect_match(list.files(dir, recursive = TRUE),
               ".*Children_[A-z]+_Count.mat$", all = TRUE)
  expect_message(fetch_plate("BB02-2E", out_dir = dir),
                 "^found 4 files under")
  rm(dat)
  gc()
  expect_false(dir.exists(dir))
})

test_that("plates can be fetched", {
  dir <- tempfile()
  expect_warning(fetch_plate("BB02-2E", data_type = "foo",
                 file_regex = ".*Children_[A-z]+_Count.mat$", out_dir = dir))
  unlink(dir, recursive = TRUE)
  expect_warning(fetch_plate("BB02-2E", plate_regex = "foo",
                 file_regex = ".*Children_[A-z]+_Count.mat$", out_dir = dir))
  unlink(dir, recursive = TRUE)
  dat <- fetch_plate("BB02-2E",
                     file_regex = ".*Children_[A-z]+_Count.mat$",
                     out_dir = dir)
  expect_length(dat, 4L)
  expect_match(dat, ".*Children_[A-z]+_Count.mat$", all = TRUE)
  expect_match(list.files(dir, recursive = TRUE),
               ".*Children_[A-z]+_Count.mat$", all = TRUE)
})

test_that("metadata can be fetched", {
  dir <- tempfile()
  expect_error(fetch_meta(out_dir = dir))
  unlink(dir, recursive = TRUE)
  expect_error(fetch_meta(type = "foo", out_dir = dir))
  meta <- fetch_meta(type = "public", out_dir = dir)
  expect_length(meta, 1L)
  expect_match(meta, ".*aggregate.csv.zip$", all = TRUE)
})
