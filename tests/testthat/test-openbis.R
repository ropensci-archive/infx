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

tok <- login_openbis(cred$username, cred$password)

test_that("openbis experiment listing works", {
  expect_is(proj <- list_projects(tok), "data.frame")
  expect_gte(nrow(proj), 1L)
  expect_named(proj)
  expect_true(all(c("spaceCode", "code") %in% names(proj)))

  expect_is(et <- list_experiment_types(tok), "data.frame")
  expect_gte(nrow(et), 1L)
  expect_named(et)
  expect_true(all(c("code", "description") %in% names(et)))

  expect_is(exp <- list_experiments(tok), "data.frame")
  expect_gte(nrow(exp), 1L)
  expect_named(exp)
  expect_true(all(c("permId", "code") %in% names(exp)))

  expect_is(exp <- list_experiments(tok, exp_type = "SIRNA_HCS"), "data.frame")
  expect_gte(nrow(exp), 1L)
  expect_named(exp)
  expect_true(all(c("permId", "code") %in% names(exp)))

  expect_is(exp <- list_experiments(tok, projects = proj[1, ]), "data.frame")
  expect_gte(nrow(exp), 1L)
  expect_named(exp)
  expect_true(all(c("permId", "code") %in% names(exp)))

  expect_is(exp <- list_experiments(tok, projects = proj[1:2, ]), "data.frame")
  expect_gte(nrow(exp), 1L)
  expect_named(exp)
  expect_true(all(c("permId", "code") %in% names(exp)))
})

test_that("openbis downloads can be created", {
  expect_is(plates <- list_plates(tok), "data.frame")
  expect_gte(nrow(plates), 1L)
  expect_named(plates)
  expect_true(all(c("plateCode", "spaceCodeOrNull") %in% names(plates)))

  expect_type(samp <- get_plate_sample(tok, "BB02-2E"), "list")
  expect_named(samp)
  expect_true(all(c("id", "permId", "identifier", "properties",
                    "retrievedFetchOptions") %in% names(samp)))
  expect_equal(length(samp[["permId"]]), 1L)

  expect_is(ds <- list_plate_datasets(tok, "BB02-2E"), "data.frame")
  expect_gte(nrow(ds), 1L)
  expect_named(ds)
  expect_true(all(c("code", "dataSetTypeCode") %in% names(ds)))

  pro <- data.frame(spaceCode = "INFECTX_PUBLISHED", code = "_COMMON")
  exp <- list_experiments(tok, pro)
  expect_is(ds <- list_exp_datasets(tok, exp[1, ]), "data.frame")
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
