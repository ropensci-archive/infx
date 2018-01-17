context("openbis list objects")

test_that("openbis experiment listing works", {
  expect_is(proj <- list_projects(tok), "json_vec")
  expect_true(all(sapply(proj, has_json_subclass, "Project")))
  expect_gte(length(proj), 1L)
  expect_equal(2 * length(proj),
               length(unlist(lapply(proj, `[`, c("spaceCode", "code")))))
})

test_that("openbis downloads can be created", {
  expect_is(plates <- list_plates(tok), "json_vec")
  expect_gte(length(plates), 1L)
  expect_true(all(sapply(plates, is_json_class)))
  expect_true(all(sapply(plates, has_json_subclass, "Plate")))
  expect_equal(2 * length(plates),
               length(unlist(lapply(plates, `[`,
                                    c("plateCode", "spaceCodeOrNull")))))
  exp <- create_exp_ids(exp_code = "VACCINIA-QU-K1",
                        proj_code = "VACCINIA_TEAM",
                        space_code = "INFECTX_PUBLISHED",
                        perm_id = "20121218143856336-1733311")
  expect_is(plates_exp <- list_plates(tok, exp), "json_vec")
  expect_true(all(sapply(plates_exp, has_json_subclass, "Plate")))
  expect_lt(length(plates_exp), length(plates))
  expect_error(list_plates(tok, "foo"))

  expect_is(samp <- get_plate_sample(tok, "BB02-2E"), "json_vec")
  expect_s3_class(samp, "json_vec")
  expect_s3_class(samp, "Sample")
  expect_true(has_json_subclass(samp[[1]], "Sample"))
  expect_true(all(c("id", "permId", "identifier", "properties",
                    "retrievedFetchOptions") %in% names(samp[[1]])))
  expect_equal(length(samp[[1]][["permId"]]), 1L)

  expect_is(ds <- list_plate_datasets(tok, "BB02-2E"), "json_vec")
  expect_true(all(sapply(ds, is_json_class)))
  expect_true(all(sapply(ds, has_json_subclass, "DataSet")))
  expect_gte(length(ds), 1L)
  expect_equal(2 * length(ds),
               length(unlist(lapply(ds, `[`, c("code", "dataSetTypeCode")))))

  expect_is(files <- list_files(tok, "20160921085125038-3519900"), "json_vec")
  expect_true(all(sapply(files, is_json_class)))
  expect_true(all(sapply(files, has_json_subclass, "FileInfoDssDTO")))
  expect_gte(length(files), 1L)
  expect_equal(2 * length(files),
               length(unlist(lapply(files, `[`,
                             c("pathInDataSet", "pathInListing")))))
})
