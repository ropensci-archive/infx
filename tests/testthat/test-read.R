context("test readers")

cred <- load_config(section = "openbis")
tok <- login_openbis(cred$username, cred$password)

test_that("matlab data files can be read", {
  files <- list_files(tok, "20160921085125038-3519900")
  sel <- grepl(paste(".*handles.mat$", ".*.xml$", ".*Cells.Parent_Nuclei.mat$",
                     ".*Count_Bacteria.mat$", ".*RobustMax_CorrDNA.mat$",
                     ".*PathName_OrigActin.mat$", sep = "|"),
               files[["pathInDataSet"]])
  dat <- do_download(tok, "20160921085125038-3519900", files[sel, ])

  expect_error(read_data(dat))
  expect_error(read_data(dat[[grep("handles", names(dat))]]))
  expect_error(read_data(dat[[grep("xml$", names(dat))]]))
  expect_type(read_data(dat[[grep("Parent_Nuclei", names(dat))]]),
                        "integer")
  expect_is(read_data(dat[[grep("RobustMax", names(dat))]]), "numeric")
  expect_true(attr(read_data(dat[[grep("RobustMax", names(dat))]]),
                   "Csingle"))
  expect_type(read_data(dat[[grep("PathName", names(dat))]]), "character")
  expect_length(read_data(dat[[grep("Count", names(dat))]]), 2304L)
  expect_true(all(attr(read_data(dat[[grep("Count", names(dat))]]),
                       "lengths") == 1))
  tmp <- read_data(dat[[grep("Parent_Nuclei", names(dat))]])
  expect_equal(length(tmp),
               sum(attr(tmp, "lengths")))
})

test_that("public metadata data files can be read", {
  public <- do_download(tok, "20140609103658114-3045667",
                     list_files(tok, "20140609103658114-3045667"))
  expect_is(dat <- read_pub_meta(public), "tbl")
  expect_gt(nrow(dat), 0L)
  expect_equal(ncol(dat), 39L)
  expect_true(all(sapply(dat, is.character)))
  expect_false(anyNA(dat[["PlateQualityStatus"]]))

  expect_warning(dat <- read_pub_meta(public, col_types = NULL))
  expect_equal(ncol(dat), 39L)
  expect_false(all(sapply(dat, is.character)))

  expect_is(dat <- read_pub_meta(public, na = c("", "NA", "NaN", "unknown",
                                                "UNKNOWN")), "tbl")
  expect_equal(ncol(dat), 39L)
  expect_true(all(sapply(dat, is.character)))
  expect_true(anyNA(dat[["PlateQualityStatus"]]))

  spec <- load_config(section = "metadata")$public
  expect_is(dat <- do.call(read_pub_meta, c(dat = list(public), spec)), "tbl")
  expect_equal(ncol(dat), length(spec$col_spec))
  expect_false(all(sapply(dat, is.character)))
})

test_that("full metadata data files can be read", {
  files <- list.files(system.file("extdata", package = "infx"),
                      pattern = "\\.tsv\\.gz$", full.names = TRUE)
  files <- setNames(lapply(files, function(x)
                      readBin(x, "raw", file.info(x)$size)),
                    basename(files))
  expect_is(dat <- read_full_meta(files), "list")
  for (i in seq_along(dat)) expect_is(dat[[i]], "tbl")
  for (i in seq_along(dat)) expect_named(dat[[i]])
  for (i in seq_along(dat)) expect_gte(nrow(dat[[i]]), 0L)
})
