context("test readers")

cred <- load_config(section = "openbis")

test_that("matlab data files can be read", {
  files <- fetch_plate(username = cred$username, password = cred$password,
                       plate_name = "BB02-2E",
                       file_regex = paste(".*handles.mat$",
                                          ".*.xml$",
                                          ".*Cells.Parent_Nuclei.mat$",
                                          ".*Count_Bacteria.mat$",
                                          ".*RobustMax_CorrDNA.mat$",
                                          ".*PathName_OrigActin.mat$",
                                          sep = "|"))
  expect_error(read_data(files))
  expect_error(read_data(files[grepl("handles", files)]))
  expect_error(read_data(files[grepl("xml$", files)]))
  expect_type(read_data(files[grepl("Parent_Nuclei", files)]), "integer")
  expect_is(read_data(files[grepl("RobustMax", files)]), "numeric")
  expect_true(attr(read_data(files[grepl("RobustMax", files)]), "Csingle"))
  expect_type(read_data(files[grepl("PathName", files)]), "character")
  expect_length(read_data(files[grepl("Count", files)]), 2304L)
  expect_true(all(attr(read_data(files[grepl("Count", files)]),
                       "lengths") == 1))
  tmp <- read_data(files[grepl("Parent_Nuclei", files)])
  expect_equal(length(tmp),
               sum(attr(tmp, "lengths")))
})

test_that("metadata data files can be read", {
  public <- fetch_meta(username = cred$username, password = cred$password,
                       type = "public")
  dat <- read_meta(public)
  expect_is(dat, "tbl")
  expect_named(dat)
  expect_true(nrow(dat) > 0)
})
