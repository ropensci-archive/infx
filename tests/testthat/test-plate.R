context("plate")

test_that("plates can be listed", {
  plate_1 <- plates
  expect_s3_class(plate_1, "Plate")
  expect_s3_class(plate_1, "json_vec")
  expect_gte(length(plate_1), 1L)
  for (i in seq_along(plate_1)) {
    expect_s3_class(plate_1[[i]], "Plate")
    expect_s3_class(plate_1[[i]], "json_class")
  }

  check_skip()

  plate_2 <- list_plates(tok, exp_ids[c(1, 2)])
  expect_s3_class(plate_2, "Plate")
  expect_s3_class(plate_2, "json_vec")
  expect_gte(length(plate_2), length(plate_1))
  for (i in seq_along(plate_2)) {
    expect_s3_class(plate_2[[i]], "Plate")
    expect_s3_class(plate_2[[i]], "json_class")
  }

  expect_identical(plate_1, list_plates(tok, experiments[[1]]))
  expect_identical(plate_2, list_plates(tok, experiments[1:2]))

  plate <- list_plates(tok)
  expect_s3_class(plate, "Plate")
  expect_s3_class(plate, "json_vec")
  expect_gte(length(plate), length(plate_2))
  for (i in seq_along(plate)) {
    expect_s3_class(plate[[i]], "Plate")
    expect_s3_class(plate[[i]], "json_class")
  }
})

test_that("wells can be listed", {
  wells_1 <- wells
  expect_s3_class(wells_1, "WellIdentifier")
  expect_s3_class(wells_1, "json_vec")
  expect_length(wells_1, 384L)
  for (i in seq_along(wells_1)) {
    expect_s3_class(wells_1[[i]], "WellIdentifier")
    expect_s3_class(wells_1[[i]], "json_class")
  }

  check_skip()

  wells_2 <- list_wells(tok, plates[c(1, 2)])
  expect_s3_class(wells_2, "WellIdentifier")
  expect_s3_class(wells_2, "json_vec")
  expect_length(wells_2, 768L)
  for (i in seq_along(wells_2)) {
    expect_s3_class(wells_2[[i]], "WellIdentifier")
    expect_s3_class(wells_2[[i]], "json_class")
  }
})

test_that("well/plate refs can be listed", {

  check_skip()

  mat <- material_id(c(2475L, 3832L), mode = "screening")

  ref_1 <- list_wells(tok, mat[[1]])
  expect_s3_class(ref_1, "PlateWellReferenceWithDatasets")
  expect_s3_class(ref_1, "json_vec")
  expect_gte(length(ref_1), 1L)
  for (i in seq_along(ref_1)) {
    expect_s3_class(ref_1[[i]], "PlateWellReferenceWithDatasets")
    expect_s3_class(ref_1[[i]], "json_class")
    expect_attr(ref_1[[i]], "mat_type")
    expect_s3_class(attr(ref_1[[i]], "mat_type"),
                    "MaterialIdentifierScreening")
    expect_s3_class(attr(ref_1[[i]], "mat_type"), "json_class")
  }

  ref_2 <- list_wells(tok, mat)
  expect_s3_class(ref_2, "PlateWellReferenceWithDatasets")
  expect_s3_class(ref_2, "json_vec")
  expect_gte(length(ref_2), length(ref_1))
  for (i in seq_along(ref_2)) {
    expect_s3_class(ref_2[[i]], "PlateWellReferenceWithDatasets")
    expect_s3_class(ref_2[[i]], "json_class")
    expect_attr(ref_2[[i]], "mat_type")
    expect_s3_class(attr(ref_2[[i]], "mat_type"),
                    "MaterialIdentifierScreening")
    expect_s3_class(attr(ref_2[[i]], "mat_type"), "json_class")
  }

  ref_1_exp <- list_wells(tok, mat[[1]], exp_ids[[1]])
  expect_s3_class(ref_1_exp, "PlateWellReferenceWithDatasets")
  expect_s3_class(ref_1_exp, "json_vec")
  expect_lte(length(ref_1_exp), length(ref_1))
  for (i in seq_along(ref_1_exp)) {
    expect_s3_class(ref_1_exp[[i]], "PlateWellReferenceWithDatasets")
    expect_s3_class(ref_1_exp[[i]], "json_class")
    expect_attr(ref_1_exp[[i]], "mat_type")
    expect_s3_class(attr(ref_1_exp[[i]], "mat_type"),
                    "MaterialIdentifierScreening")
    expect_s3_class(attr(ref_1_exp[[i]], "mat_type"), "json_class")
  }

  expect_identical(ref_1_exp, list_wells(tok, mat[[1]], exp_ids[1]))

  ref_2_exp <- list_wells(tok, mat, exp_ids[[1]])
  expect_s3_class(ref_2_exp, "PlateWellReferenceWithDatasets")
  expect_s3_class(ref_2_exp, "json_vec")
  expect_lte(length(ref_2_exp), length(ref_2))
  for (i in seq_along(ref_2_exp)) {
    expect_s3_class(ref_2_exp[[i]], "PlateWellReferenceWithDatasets")
    expect_s3_class(ref_2_exp[[i]], "json_class")
    expect_attr(ref_2_exp[[i]], "mat_type")
    expect_s3_class(attr(ref_2_exp[[i]], "mat_type"),
                    "MaterialIdentifierScreening")
    expect_s3_class(attr(ref_2_exp[[i]], "mat_type"), "json_class")
  }

  expect_identical(ref_2_exp,
                   list_wells(tok, mat[[1]], experiments[1]))
})

test_that("plate metadata be listed", {

  check_skip()

  plate_ids <- as_plate_id(plates)

  meta_1 <- list_plate_metadata(tok, plate_ids[[1]])
  expect_s3_class(meta_1, "PlateMetadata")
  expect_s3_class(meta_1, "json_class")
  expect_true(has_fields(meta_1, "wells"))
  expect_length(meta_1[["wells"]], 384L)
  expect_s3_class(meta_1[["wells"]], "WellMetadata")
  expect_s3_class(meta_1[["wells"]], "json_vec")
  for (i in seq_along(meta_1[["wells"]])) {
    expect_s3_class(meta_1[["wells"]][[i]], "WellMetadata")
    expect_s3_class(meta_1[["wells"]][[i]], "json_class")
  }

  meta_2 <- list_plate_metadata(tok, plate_ids[1:2])
  expect_s3_class(meta_2, "PlateMetadata")
  expect_s3_class(meta_2, "json_vec")
  expect_length(meta_2, 2L)
  for (i in seq_along(meta_2)) {
    expect_s3_class(meta_2[[i]], "PlateMetadata")
    expect_s3_class(meta_2[[i]], "json_class")
    expect_true(has_fields(meta_2[[i]], "wells"))
    expect_length(meta_2[[i]][["wells"]], 384L)
    expect_s3_class(meta_2[[i]][["wells"]], "WellMetadata")
    expect_s3_class(meta_2[[i]][["wells"]], "json_vec")
    for (j in seq_along(meta_2[[i]][["wells"]])) {
      expect_s3_class(meta_2[[i]][["wells"]][[j]], "WellMetadata")
      expect_s3_class(meta_2[[i]][["wells"]][[j]], "json_class")
    }
  }

  expect_identical(list_plate_metadata(tok, plates[[1]]), meta_1)
  expect_identical(list_plate_metadata(tok, plates[1:2]), meta_2)
})

test_that("plates/samples can be converted to plate ids", {

  plate_id <- as_plate_id(plates[[1]])
  expect_s3_class(plate_id, "PlateIdentifier")
  expect_s3_class(plate_id, "json_class")
  expect_true(has_fields(plate_id, c("plateCode", "spaceCodeOrNull")))
  expect_is(plate_id[["plateCode"]], "character")
  expect_is(plate_id[["spaceCodeOrNull"]], "character")

  plate_ids <- as_plate_id(plates[1:2])
  expect_s3_class(plate_ids, "PlateIdentifier")
  expect_s3_class(plate_ids, "json_vec")
  expect_length(plate_ids, 2L)
  for (i in seq_along(plate_ids)) {
    expect_s3_class(plate_ids[[i]], "PlateIdentifier")
    expect_s3_class(plate_ids[[i]], "json_class")
    expect_true(has_fields(plate_ids[[i]], c("plateCode", "spaceCodeOrNull")))
    expect_is(plate_ids[[i]][["plateCode"]], "character")
    expect_is(plate_ids[[i]][["spaceCodeOrNull"]], "character")
  }

  plate_id <- as_plate_id(samples[[1]])
  expect_s3_class(plate_id, "PlateIdentifier")
  expect_s3_class(plate_id, "json_class")
  expect_true(has_fields(plate_id, c("plateCode", "spaceCodeOrNull")))
  expect_is(plate_id[["plateCode"]], "character")
  expect_is(plate_id[["spaceCodeOrNull"]], "character")

  plate_ids <- as_plate_id(samples[1:2])
  expect_s3_class(plate_ids, "PlateIdentifier")
  expect_s3_class(plate_ids, "json_vec")
  expect_length(plate_ids, 2L)
  for (i in seq_along(plate_ids)) {
    expect_s3_class(plate_ids[[i]], "PlateIdentifier")
    expect_s3_class(plate_ids[[i]], "json_class")
    expect_true(has_fields(plate_ids[[i]], c("plateCode", "spaceCodeOrNull")))
    expect_is(plate_ids[[i]][["plateCode"]], "character")
    expect_is(plate_ids[[i]][["spaceCodeOrNull"]], "character")
  }
})

test_that("well position objects can be created", {
  pos_1 <- well_pos(1L, 1L)
  expect_s3_class(pos_1, "WellPosition")
  expect_s3_class(pos_1, "json_class")
  expect_true(has_fields(pos_1, c("wellRow", "wellColumn")))
  expect_is(pos_1[["wellRow"]], "integer")
  expect_is(pos_1[["wellColumn"]], "integer")

  pos_2 <- well_pos(1L, 1L:2L)
  expect_s3_class(pos_2, "WellPosition")
  expect_s3_class(pos_2, "json_vec")
  expect_length(pos_2, 2L)
  for (i in seq_along(pos_2)) {
    expect_s3_class(pos_2[[i]], "WellPosition")
    expect_s3_class(pos_2[[i]], "json_class")
    expect_true(has_fields(pos_2[[i]], c("wellRow", "wellColumn")))
    expect_is(pos_2[[i]][["wellRow"]], "integer")
    expect_is(pos_2[[i]][["wellColumn"]], "integer")
  }

  pos_2 <- well_pos(2L:3L, 1L:2L)
  expect_s3_class(pos_2, "WellPosition")
  expect_s3_class(pos_2, "json_vec")
  expect_length(pos_2, 2L)
  for (i in seq_along(pos_2)) {
    expect_s3_class(pos_2[[i]], "WellPosition")
    expect_s3_class(pos_2[[i]], "json_class")
    expect_true(has_fields(pos_2[[i]], c("wellRow", "wellColumn")))
    expect_is(pos_2[[i]][["wellRow"]], "integer")
    expect_is(pos_2[[i]][["wellColumn"]], "integer")
  }

  pos_1 <- well_pos("a", 1L)
  expect_s3_class(pos_1, "WellPosition")
  expect_s3_class(pos_1, "json_class")
  expect_true(has_fields(pos_1, c("wellRow", "wellColumn")))
  expect_is(pos_1[["wellRow"]], "integer")
  expect_is(pos_1[["wellColumn"]], "integer")

  expect_error(well_pos(c("a", "b"), 1L:3L))
  expect_error(well_pos(1L:2L, c("a", "b")))
})