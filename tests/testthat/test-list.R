context("list misc objects")

test_that("projects can be listed", {
  proj <- list_projects(tok)
  expect_is(proj, "Project")
  expect_is(proj, "json_vec")
  expect_identical(get_common_subclass(proj), "Project")
  expect_true(all(sapply(proj, has_subclass, "Project")))
  expect_gte(length(proj), 1L)
})

test_that("openbis downloads can be created", {
  expect_is(plates <- list_plates(tok), "json_vec")
  expect_gte(length(plates), 1L)
  expect_true(all(sapply(plates, is_json_class)))
  expect_true(all(sapply(plates, has_subclass, "Plate")))
  expect_equal(2 * length(plates),
               length(unlist(lapply(plates, `[`,
                                    c("plateCode", "spaceCodeOrNull")))))
  exp <- create_exp_ids(exp_code = "VACCINIA-QU-K1",
                        proj_code = "VACCINIA_TEAM",
                        space_code = "INFECTX_PUBLISHED",
                        perm_id = "20121218143856336-1733311")
  expect_is(plates_exp <- list_plates(tok, exp), "json_vec")
  expect_true(all(sapply(plates_exp, has_subclass, "Plate")))
  expect_lt(length(plates_exp), length(plates))
  expect_error(list_plates(tok, "foo"))

  expect_is(files <- list_files(tok, "20160921085125038-3519900"), "json_vec")
  expect_true(all(sapply(files, is_json_class)))
  expect_true(all(sapply(files, has_subclass, "FileInfoDssDTO")))
  expect_gte(length(files), 1L)
  expect_equal(2 * length(files),
               length(unlist(lapply(files, `[`,
                             c("pathInDataSet", "pathInListing")))))
})
