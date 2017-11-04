context("openbis downloader")

cred <- load_config(section = "openbis")
tok <- login_openbis(cred$username, cred$password)

test_that("plate data can be fetched", {
  expect_type(dat <- fetch_plate(tok, "BB02-2E",
                                 file_regex = "^Image\\.Count_"), "list")
  expect_named(dat)
  expect_gte(length(dat), 1L)
  expect_true(all(sapply(dat, is.raw)))
  expect_error(fetch_plate(tok, "foobar", file_regex = "^Image\\.Count_"))
  expect_error(fetch_plate(tok, "BB02-2E", file_regex = "^foobar$"))
})

test_that("metadata can be fetched", {
  expect_error(fetch_meta(tok))
  expect_type(dat <- fetch_meta(tok, "public"), "list")
  expect_named(dat)
  expect_equal(length(dat), 1L)
  expect_type(dat[[1]], "raw")
  expect_true(grepl("\\.csv\\.zip$", names(dat)))
})
