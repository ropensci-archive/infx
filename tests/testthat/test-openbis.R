context("openbis downloader")

test_that("openbis system call", {
  expect_error(fetch_obis())
  dir <- tempfile()
  expect_true(dir.create(dir))
  tmp <- fetch_openbis(data_id = "20150421161324410-3129549", out_dir = dir)
  expect_true(any(grepl("^Successfully downloaded 2 files", tmp)))
  tmp <- fetch_openbis(plate_id = "^/.*/BB02-2E$",
                       data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                       file_regex = ".*Children_[A-z]+_Count.mat$",
                       out_dir = dir)
  expect_true(any(grepl("does not exist, will download.$", tmp)))
})
