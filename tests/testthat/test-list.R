context("openbis list objects")

test_that("openbis experiment listing works", {
  expect_is(proj <- list_projects(tok), "list")
  expect_true(all(sapply(proj, has_json_class, "Project")))
  expect_gte(length(proj), 1L)
  expect_equal(2 * length(proj),
               length(unlist(lapply(proj, `[`, c("spaceCode", "code")))))

  expect_is(et <- list_experiment_types(tok), "list")
  expect_true(all(sapply(et, has_json_class, "ExperimentType")))
  expect_gte(length(et), 1L)
  expect_equal(2 * length(et),
               length(unlist(lapply(et, `[`, c("code", "description")))))

  expect_is(exp_ids <- list_experiment_ids(tok), "list")
  expect_true(all(sapply(exp_ids, has_json_class, "ExperimentIdentifier")))
  expect_gte(length(exp_ids), 1L)
  expect_equal(2 * length(exp_ids),
               length(unlist(lapply(exp_ids, `[`, c("permId", "spaceCode")))))

  expect_is(exp <- list_experiments(tok), "list")
  expect_true(all(sapply(exp, has_json_class, "Experiment")))
  expect_gte(length(exp), 1L)
  expect_equal(2 * length(exp),
               length(unlist(lapply(exp, `[`, c("permId", "code")))))

  expect_is(exp <- list_experiments(tok, exp_type = "SIRNA_HCS"), "list")
  expect_true(all(sapply(exp, has_json_class, "Experiment")))
  expect_gte(length(exp), 1L)
  expect_equal(2 * length(exp),
               length(unlist(lapply(exp, `[`, c("permId", "code")))))

  expect_is(exp <- list_experiments(tok, projects = proj[1]), "list")
  expect_true(all(sapply(exp, has_json_class, "Experiment")))
  expect_gte(length(exp), 1L)
  expect_equal(2 * length(exp),
               length(unlist(lapply(exp, `[`, c("permId", "code")))))

  expect_equal(list_experiments(tok, projects = proj[1]),
               list_experiments(tok, projects = proj[[1]]))

  expect_is(exp <- list_experiments(tok, projects = proj[1:2]), "list")
  expect_true(all(sapply(exp, has_json_class, "Experiment")))
  expect_gte(length(exp), 1L)
  expect_equal(2 * length(exp),
               length(unlist(lapply(exp, `[`, c("permId", "code")))))

  expect_warning(list_experiments(tok, exp_type = "SIRNA_HCS",
                                  exp_ids = exp_ids[1:2]))
  expect_equal(list_experiments(tok, exp_ids = exp_ids[[1]]),
               list_experiments(tok, exp_ids =
    paste0("/", exp_ids[[1]][c("spaceCode", "projectCode", "experimentCode")],
           collapse = "")))
  expect_is(exp <- list_experiments(tok, exp_ids = exp_ids[1:2]), "list")
  expect_true(all(sapply(exp, has_json_class, "Experiment")))
  expect_gte(length(exp), 1L)
  expect_equal(2 * length(exp),
               length(unlist(lapply(exp, `[`, c("permId", "code")))))
})

test_that("openbis downloads can be created", {
  expect_is(plates <- list_plates(tok), "list")
  expect_gte(length(plates), 1L)
  expect_true(all(sapply(plates, is_json_class)))
  expect_true(all(sapply(plates, has_json_class, "Plate")))
  expect_equal(2 * length(plates),
               length(unlist(lapply(plates, `[`,
                                    c("plateCode", "spaceCodeOrNull")))))

  expect_type(samp <- get_plate_sample(tok, "BB02-2E"), "list")
  expect_s3_class(samp, "json_class")
  expect_true(has_json_class(samp, "Sample"))
  expect_true(all(c("id", "permId", "identifier", "properties",
                    "retrievedFetchOptions") %in% names(samp)))
  expect_equal(length(samp[["permId"]]), 1L)

  expect_is(ds <- list_plate_datasets(tok, "BB02-2E"), "list")
  expect_true(all(sapply(ds, is_json_class)))
  expect_true(all(sapply(ds, has_json_class, "DataSet")))
  expect_gte(length(ds), 1L)
  expect_equal(2 * length(ds),
               length(unlist(lapply(ds, `[`, c("code", "dataSetTypeCode")))))

  pro <- structure(list(spaceCode = "INFECTX_PUBLISHED", code = "_COMMON"),
                   class = "json_class", json_class = "Project")
  exp <- list_experiments(tok, pro)
  expect_is(ds <- list_exp_datasets(tok, exp[[1]]), "list")
  expect_true(all(sapply(ds, is_json_class)))
  expect_true(all(sapply(ds, has_json_class, "DataSet")))
  expect_gte(length(ds), 1L)
  expect_equal(2 * length(ds),
               length(unlist(lapply(ds, `[`, c("code", "dataSetTypeCode")))))

  expect_is(files <- list_files(tok, "20160921085125038-3519900"), "list")
  expect_true(all(sapply(files, is_json_class)))
  expect_true(all(sapply(files, has_json_class, "FileInfoDssDTO")))
  expect_gte(length(files), 1L)
  expect_equal(2 * length(files),
               length(unlist(lapply(files, `[`,
                             c("pathInDataSet", "pathInListing")))))
})
