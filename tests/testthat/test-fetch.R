context("openbis downloader")

cred <- load_config(section = "openbis")
tok <- login_openbis(cred$username, cred$password)

test_that("openbis downloads can be executed", {
  files <- list_files(tok, "20160921085125038-3519900")

  expect_error(do_download(tok, "20160921085125038-3519900", "foo"))
  expect_error(do_download(tok, "20160921085125038-3519900", files[254, 1:3]))
  expect_error(do_download(tok, "20160921085125038-3519900", files))

  dat <- do_download(tok, "20160921085125038-3519900", files[254, ])
  expect_type(dat, "list")
  expect_named(dat)
  expect_equal(length(dat), 1L)
  expect_type(dat[[1]], "raw")
  expect_true(grepl(names(dat), files[254, "pathInDataSet"]))

  dat <- do_download(tok, "20160921085125038-3519900", files[254:260, ])
  expect_type(dat, "list")
  expect_named(dat)
  expect_equal(length(dat), length(254:260))
  expect_true(all(sapply(dat, is.raw)))
})

test_that("plate data can be fetched", {
  expect_type(dat <- fetch_plate(tok, "BB02-2E",
                                 file_regex = "^Image\\.Count_"), "list")
  expect_named(dat)
  expect_gte(length(dat), 1L)
  expect_true(all(sapply(dat, is.numeric)))
  expect_named(attributes(dat[[1]]), c("lengths", "object", "feature"))
  expect_error(fetch_plate(tok, "foobar", file_regex = "^Image\\.Count_"))
  expect_error(fetch_plate(tok, "BB02-2E", file_regex = "^foobar$"))
  dat <- fetch_plate(tok, plate_id = "BB02-2E",
                     file_regex = paste(".*handles.mat$",
                                        ".*Cells.Parent_Nuclei.mat$",
                                        ".*Count_Bacteria.mat$",
                                        ".*RobustMax_CorrDNA.mat$",
                                        ".*PathName_OrigActin.mat$",
                                        sep = "|"))
  expect_equal(length(dat), 4L)
})

test_that("metadata can be fetched", {
  expect_error(fetch_meta(tok))
  expect_type(dat <- fetch_meta(tok, "public"), "list")
  expect_is(dat, "tbl")
  expect_named(dat)
  expect_true(nrow(dat) > 0)
})
