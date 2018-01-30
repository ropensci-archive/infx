context("plate")

test_that("plates can be listed", {
  plate <- list_plates(tok)
  expect_is(plate, "Plate")
  expect_is(plate, "json_vec")
  expect_identical(get_common_subclass(plate), "Plate")
  expect_true(all(sapply(plate, has_json_subclass, "Plate")))
  expect_gte(length(plate), 1L)

  exp_ids <- list_experiment_ids(tok)
  experiments <- list_experiments(tok, exp_ids[1:2])

  plate_1 <- list_plates(tok, exp_ids[[1]])
  expect_is(plate_1, "Plate")
  expect_is(plate_1, "json_vec")
  expect_identical(get_common_subclass(plate_1), "Plate")
  expect_true(all(sapply(plate_1, has_json_subclass, "Plate")))
  expect_gte(length(plate_1), 1L)

  plate_2 <- list_plates(tok, exp_ids[c(1, 2)])
  expect_is(plate_2, "Plate")
  expect_is(plate_2, "json_vec")
  expect_identical(get_common_subclass(plate_2), "Plate")
  expect_true(all(sapply(plate_2, has_json_subclass, "Plate")))
  expect_gte(length(plate_2), length(plate_1))

  expect_identical(plate_1, list_plates(tok, experiments[[1]]))
  expect_identical(plate_2, list_plates(tok, experiments[1:2]))
})

test_that("wells can be listed", {
  exp_ids <- list_experiment_ids(tok)
  plate <- list_plates(tok, exp_ids[[1]])

  wells_1 <- list_wells(tok, plate[[1]])
  expect_is(wells_1, "WellIdentifier")
  expect_is(wells_1, "json_vec")
  expect_identical(get_common_subclass(wells_1), "WellIdentifier")
  expect_true(all(sapply(wells_1, has_json_subclass, "WellIdentifier")))
  expect_equal(length(wells_1), 384L)

  wells_2 <- list_wells(tok, plate[c(1, 2)])
  expect_is(wells_2, "WellIdentifier")
  expect_is(wells_2, "json_vec")
  expect_identical(get_common_subclass(wells_2), "WellIdentifier")
  expect_true(all(sapply(wells_2, has_json_subclass, "WellIdentifier")))
  expect_equal(length(wells_2), 768L)
})

test_that("well/plate refs can be listed", {
  mat <- material_id(c(2475L, 3832L), mode = "screening")

  ref_1 <- list_plate_well_ref(tok, mat[[1]])
  expect_is(ref_1, "PlateWellReferenceWithDatasets")
  expect_is(ref_1, "json_vec")
  expect_identical(get_common_subclass(ref_1),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_1, has_json_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ref_1), 1L)

  ref_2 <- list_plate_well_ref(tok, mat)
  expect_is(ref_2, "PlateWellReferenceWithDatasets")
  expect_is(ref_2, "json_vec")
  expect_identical(get_common_subclass(ref_2),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_2, has_json_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ref_2), length(ref_1))


  exp_ids <- list_experiment_ids(tok)

  ref_1_exp <- list_plate_well_ref(tok, mat[[1]], exp_ids[[1]])
  expect_is(ref_1_exp, "PlateWellReferenceWithDatasets")
  expect_is(ref_1_exp, "json_vec")
  expect_identical(get_common_subclass(ref_1_exp),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_1_exp, has_json_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_lte(length(ref_1_exp), length(ref_1))
  expect_identical(ref_1_exp, list_plate_well_ref(tok, mat[[1]], exp_ids[1]))

  ref_2_exp <- list_plate_well_ref(tok, mat, exp_ids[[1]])
  expect_is(ref_2_exp, "PlateWellReferenceWithDatasets")
  expect_is(ref_2_exp, "json_vec")
  expect_identical(get_common_subclass(ref_2_exp),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_2_exp, has_json_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_lte(length(ref_2_exp), length(ref_2))

  expect_identical(ref_2_exp,
                   list_plate_well_ref(tok, mat[[1]],
                                       list_experiments(tok, exp_ids[1])))
})

test_that("plate metadata be listed", {
  exp_ids <- list_experiment_ids(tok)
  plates <- list_plates(tok, exp_ids[[1]])
  plate_ids <- plate_to_plateid(plates)

  meta_1 <- list_plate_metadata(tok, plate_ids[[1]])
  expect_is(meta_1, "PlateMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_common_subclass(meta_1), "PlateMetadata")
  expect_true(all(sapply(meta_1, has_json_subclass, "PlateMetadata")))
  expect_equal(length(meta_1), 1L)

  meta_2 <- list_plate_metadata(tok, plate_ids[1:2])
  expect_is(meta_2, "PlateMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_common_subclass(meta_2), "PlateMetadata")
  expect_true(all(sapply(meta_2, has_json_subclass, "PlateMetadata")))
  expect_equal(length(meta_2), 2L)

  expect_identical(list_plate_metadata(tok, plates[[1]]), meta_1)
  expect_identical(list_plate_metadata(tok, plates[1:2]), meta_2)
})
