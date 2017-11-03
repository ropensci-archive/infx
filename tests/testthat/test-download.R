context("openbis downloader")

cred <- load_config(section = "openbis")

test_that("openbis login is possible", {
  expect_error(login_openbis())
  expect_error(login_openbis("foo", "bar"))
  expect_error(login_openbis(cred$username, cred$password, api = "foo"))
  expect_error(login_openbis(cred$username, cred$password, url = "foo"))
  expect_error(login_openbis("foo", "bar", url = "google.ch"))
  expect_error(login_openbis("foo", "bar", foo = "google.ch"))
  expect_type(tok <- login_openbis(cred$username, cred$password,
                                   auto_disconnect = FALSE), "character")
  expect_true(grepl(paste0("^", cred$username), tok))
  expect_true(is_token_valid(tok))
  expect_null(logout_openbis(tok))
  expect_false(is_token_valid(tok))
  tok <- login_openbis(cred$username, cred$password,
                       url = "https://infectx.biozentrum.unibas.ch")
  tok_chr <- as.character(tok)
  expect_true(is_token_valid(tok_chr))
  rm(tok)
  gc()
  expect_false(is_token_valid(tok_chr))
})

test_that("openbis download is possible", {
  tok <- login_openbis(cred$username, cred$password)
  expect_is(plates <- list_plates(tok), "data.frame")
  expect_gte(nrow(plates), 1L)
  expect_named(plates)
  expect_true(all(c("plateCode", "spaceCodeOrNull") %in% names(plates)))

  expect_type(samp <- get_plate_sample(tok, "BB02-2E"), "list")
  expect_named(samp)
  expect_true(all(c("id", "permId", "identifier", "properties",
                    "retrievedFetchOptions") %in% names(samp)))
  expect_equal(length(samp[["permId"]]), 1L)

  expect_is(ds <- list_datasets(tok, "BB02-2E"), "data.frame")
  expect_gte(nrow(ds), 1L)
  expect_named(ds)
  expect_true(all(c("code", "dataSetTypeCode") %in% names(ds)))

  expect_is(files <- list_files(tok, "20160921085125038-3519900"),
            "data.frame")
  expect_gte(nrow(files), 1L)
  expect_named(files)
  expect_true(all(c("pathInDataSet", "pathInListing") %in% names(files)))

  expect_type(link <- get_download(tok, "20160921085125038-3519900",
                                   files[["pathInDataSet"]][2]),
              "character")
  expect_true(grepl("^https://", link))
})

test_that("openbis system call works", {
  expect_error(fetch_openbis())
  expect_error(fetch_openbis(cred$username))
  expect_error(fetch_openbis(cred$username, cred$password))
  expect_error(fetch_openbis(cred$username, cred$password, verbosity = -1))
  expect_error(fetch_openbis(cred$username, cred$password,
                             result_class = "foo"))
  expect_error(fetch_openbis(cred$username, cred$password,
                             out_dir = "foo"))
  dir <- tempfile()
  expect_true(dir.create(dir))
  expect_error(fetch_openbis(cred$username, cred$password,
               data_id = "foobar", out_dir = dir))
  tmp <- fetch_openbis(cred$username, cred$password,
                       data_id = "20150421161324410-3129549", out_dir = dir)
  expect_match(tmp, "^Successfully downloaded 2 files", all = FALSE)
  tmp <- fetch_openbis(cred$username, cred$password,
                       plate_regex = "^/.*/BB02-2E$",
                       data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                       file_regex = ".*Children_[A-z]+_Count.mat$",
                       out_dir = dir)
  expect_match(tmp, "does not exist, will download.$", all = FALSE)
  unlink(dir, recursive = TRUE)
})

test_that("data can be fetched", {
  dir <- tempfile()
  expect_true(dir.create(dir))
  expect_error(fetch_data(username = cred$username, password = cred$password,
                          plate_regex = "^/.*/BB02-2E$",
                          data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                          file_regex = ".*Children_[A-z]+_Count.mat$",
                          out_dir = dir))
  unlink(dir, recursive = TRUE)
  expect_warning(fetch_data(username = cred$username, password = cred$password,
                            plate_regex = "^/.*/BB02-2E$",
                            data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                            file_regex = ".*Children_[A-z]+_Count.mat$",
                            out_dir = dir, verbosity = 12))
  unlink(dir, recursive = TRUE)
  dat <- fetch_data(username = cred$username, password = cred$password,
                    plate_regex = "^/.*/BB02-2E$",
                    data_type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                    file_regex = ".*Children_[A-z]+_Count.mat$",
                    out_dir = dir)
  expect_length(dat, 4L)
  expect_match(dat, ".*Children_[A-z]+_Count.mat$", all = TRUE)
  expect_match(list.files(dir, recursive = TRUE),
               ".*Children_[A-z]+_Count.mat$", all = TRUE)
  expect_message(fetch_plate(username = cred$username,
                             password = cred$password,
                             plate_name = "BB02-2E", out_dir = dir),
                 "^found 4 files under")
  rm(dat)
  gc()
  expect_false(dir.exists(dir))
})

test_that("plates can be fetched", {
  dir <- tempfile()
  expect_warning(fetch_plate(username = cred$username,
                             password = cred$password,
                             plate_name = "BB02-2E", data_type = "foo",
                 file_regex = ".*Children_[A-z]+_Count.mat$", out_dir = dir))
  unlink(dir, recursive = TRUE)
  expect_warning(fetch_plate(username = cred$username,
                             password = cred$password,
                             plate_name = "BB02-2E", plate_regex = "foo",
                 file_regex = ".*Children_[A-z]+_Count.mat$", out_dir = dir))
  unlink(dir, recursive = TRUE)
  dat <- fetch_plate(username = cred$username, password = cred$password,
                     plate_name = "BB02-2E",
                     file_regex = ".*Children_[A-z]+_Count.mat$",
                     out_dir = dir)
  expect_length(dat, 4L)
  expect_match(dat, ".*Children_[A-z]+_Count.mat$", all = TRUE)
  expect_match(list.files(dir, recursive = TRUE),
               ".*Children_[A-z]+_Count.mat$", all = TRUE)
})

test_that("metadata can be fetched", {
  dir <- tempfile()
  expect_error(fetch_meta(username = cred$username, password = cred$password,
                          out_dir = dir))
  unlink(dir, recursive = TRUE)
  expect_error(fetch_meta(username = cred$username, password = cred$password,
                          type = "foo", out_dir = dir))
  meta <- fetch_meta(username = cred$username, password = cred$password,
                     type = "public", out_dir = dir)
  expect_length(meta, 1L)
  expect_match(meta, ".*aggregate.csv.zip$", all = TRUE)
})
