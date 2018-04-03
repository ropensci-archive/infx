context("plate")

test_that("plates can be listed", {
  plate_1 <- plates
  expect_is(plate_1, "Plate")
  expect_is(plate_1, "json_vec")
  expect_identical(get_subclass(plate_1), "Plate")
  expect_true(all(sapply(plate_1, has_subclass, "Plate")))
  expect_gte(length(plate_1), 1L)

  check_skip()

  plate_2 <- list_plates(tok, exp_ids[c(1, 2)])
  expect_is(plate_2, "Plate")
  expect_is(plate_2, "json_vec")
  expect_identical(get_subclass(plate_2), "Plate")
  expect_true(all(sapply(plate_2, has_subclass, "Plate")))
  expect_gte(length(plate_2), length(plate_1))

  expect_identical(plate_1, list_plates(tok, experiments[[1]]))
  expect_identical(plate_2, list_plates(tok, experiments[1:2]))

  plate <- list_plates(tok)
  expect_is(plate, "Plate")
  expect_is(plate, "json_vec")
  expect_identical(get_subclass(plate), "Plate")
  expect_true(all(sapply(plate, has_subclass, "Plate")))
  expect_gte(length(plate), 1L)
})

test_that("wells can be listed", {
  wells_1 <- wells
  expect_is(wells_1, "WellIdentifier")
  expect_is(wells_1, "json_vec")
  expect_identical(get_subclass(wells_1), "WellIdentifier")
  expect_true(all(sapply(wells_1, has_subclass, "WellIdentifier")))
  expect_equal(length(wells_1), 384L)

  check_skip()

  wells_2 <- list_wells(tok, plates[c(1, 2)])
  expect_is(wells_2, "WellIdentifier")
  expect_is(wells_2, "json_vec")
  expect_identical(get_subclass(wells_2), "WellIdentifier")
  expect_true(all(sapply(wells_2, has_subclass, "WellIdentifier")))
  expect_equal(length(wells_2), 768L)
})

test_that("well/plate refs can be listed", {

  check_skip()

  mat <- material_id(c(2475L, 3832L), mode = "screening")

  ref_1 <- list_plate_well_ref(tok, mat[[1]])
  expect_is(ref_1, "PlateWellReferenceWithDatasets")
  expect_is(ref_1, "json_vec")
  expect_identical(get_subclass(ref_1),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_1, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ref_1), 1L)

  ref_2 <- list_plate_well_ref(tok, mat)
  expect_is(ref_2, "PlateWellReferenceWithDatasets")
  expect_is(ref_2, "json_vec")
  expect_identical(get_subclass(ref_2),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_2, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_gte(length(ref_2), length(ref_1))

  ref_1_exp <- list_plate_well_ref(tok, mat[[1]], exp_ids[[1]])
  expect_is(ref_1_exp, "PlateWellReferenceWithDatasets")
  expect_is(ref_1_exp, "json_vec")
  expect_identical(get_subclass(ref_1_exp),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_1_exp, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_lte(length(ref_1_exp), length(ref_1))
  expect_identical(ref_1_exp, list_plate_well_ref(tok, mat[[1]], exp_ids[1]))

  ref_2_exp <- list_plate_well_ref(tok, mat, exp_ids[[1]])
  expect_is(ref_2_exp, "PlateWellReferenceWithDatasets")
  expect_is(ref_2_exp, "json_vec")
  expect_identical(get_subclass(ref_2_exp),
                   "PlateWellReferenceWithDatasets")
  expect_true(all(sapply(ref_2_exp, has_subclass,
                         "PlateWellReferenceWithDatasets")))
  expect_lte(length(ref_2_exp), length(ref_2))

  expect_identical(ref_2_exp,
                   list_plate_well_ref(tok, mat[[1]], experiments[1]))
})

test_that("plate metadata be listed", {

  check_skip()

  plate_ids <- as_plateid(plates)

  meta_1 <- list_plate_metadata(tok, plate_ids[[1]])
  expect_is(meta_1, "PlateMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_subclass(meta_1), "PlateMetadata")
  expect_true(all(sapply(meta_1, has_subclass, "PlateMetadata")))
  expect_equal(length(meta_1), 1L)

  meta_2 <- list_plate_metadata(tok, plate_ids[1:2])
  expect_is(meta_2, "PlateMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_subclass(meta_2), "PlateMetadata")
  expect_true(all(sapply(meta_2, has_subclass, "PlateMetadata")))
  expect_equal(length(meta_2), 2L)

  expect_identical(list_plate_metadata(tok, plates[[1]]), meta_1)
  expect_identical(list_plate_metadata(tok, plates[1:2]), meta_2)
})

test_that("plates/samples can be converted to plate ids", {

  plate_id <- as_plateid(plates[[1]])
  expect_is(plate_id, "PlateIdentifier")
  expect_is(plate_id, "json_vec")
  expect_identical(get_subclass(plate_id), "PlateIdentifier")
  expect_true(all(sapply(plate_id, has_subclass, "PlateIdentifier")))
  expect_true(all(sapply(plate_id, has_fields,
                         c("plateCode", "spaceCodeOrNull"))))
  expect_equal(length(plate_id), 1L)

  plate_ids <- as_plateid(plates[1:2])
  expect_is(plate_ids, "PlateIdentifier")
  expect_is(plate_ids, "json_vec")
  expect_identical(get_subclass(plate_ids), "PlateIdentifier")
  expect_true(all(sapply(plate_ids, has_subclass, "PlateIdentifier")))
  expect_true(all(sapply(plate_ids, has_fields,
                         c("plateCode", "spaceCodeOrNull"))))
  expect_equal(length(plate_ids), 2L)

  plate_id <- as_plateid(samples[[1]])
  expect_is(plate_id, "PlateIdentifier")
  expect_is(plate_id, "json_vec")
  expect_identical(get_subclass(plate_id), "PlateIdentifier")
  expect_true(all(sapply(plate_id, has_subclass, "PlateIdentifier")))
  expect_true(all(sapply(plate_id, has_fields,
                         c("plateCode", "spaceCodeOrNull"))))
  expect_equal(length(plate_id), 1L)

  plate_ids <- as_plateid(samples[1:2])
  expect_is(plate_ids, "PlateIdentifier")
  expect_is(plate_ids, "json_vec")
  expect_identical(get_subclass(plate_ids), "PlateIdentifier")
  expect_true(all(sapply(plate_ids, has_subclass, "PlateIdentifier")))
  expect_true(all(sapply(plate_ids, has_fields,
                         c("plateCode", "spaceCodeOrNull"))))
  expect_equal(length(plate_ids), 2L)
})

test_that("well position objects can be created", {
  pos_1 <- well_pos(1L, 1L)

  expect_is(pos_1, "WellPosition")
  expect_is(pos_1, "json_vec")
  expect_identical(get_subclass(pos_1), "WellPosition")
  expect_true(all(sapply(pos_1, has_subclass, "WellPosition")))
  expect_length(pos_1, 1L)

  pos_2 <- well_pos(1L, 1L:2L)

  expect_is(pos_2, "WellPosition")
  expect_is(pos_2, "json_vec")
  expect_identical(get_subclass(pos_2), "WellPosition")
  expect_true(all(sapply(pos_2, has_subclass, "WellPosition")))
  expect_length(pos_2, 2L)

  pos_2 <- well_pos(2L:3L, 1L:2L)

  expect_is(pos_2, "WellPosition")
  expect_is(pos_2, "json_vec")
  expect_identical(get_subclass(pos_2), "WellPosition")
  expect_true(all(sapply(pos_2, has_subclass, "WellPosition")))
  expect_length(pos_2, 2L)

  pos_1 <- well_pos("a", 1L)

  expect_is(pos_1, "WellPosition")
  expect_is(pos_1, "json_vec")
  expect_identical(get_subclass(pos_1), "WellPosition")
  expect_true(all(sapply(pos_1, has_subclass, "WellPosition")))
  expect_length(pos_1, 1L)

  expect_error(well_pos(c("a", "b"), 1L:3L))
  expect_error(well_pos(1L:2L, c("a", "b")))
})