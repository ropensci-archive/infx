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

test_that("metadata data files can be read", {
  public <- do_download(tok, "20140609103658114-3045667",
                     list_files(tok, "20140609103658114-3045667"))
  dat <- read_pub_meta(public)
  expect_is(dat, "tbl")
  expect_named(dat)
  expect_true(nrow(dat) > 0)
})
