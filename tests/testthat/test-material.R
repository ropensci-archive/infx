context("list materials")

test_that("material ids can be created", {

  material <- material_id(2475L, type = "gene")
  expect_s3_class(material, "MaterialIdentifierScreening")
  expect_s3_class(material, "json_class")
  expect_true(has_fields(material,
                         c("materialTypeIdentifier", "materialCode")))
  expect_s3_class(material[["materialTypeIdentifier"]],
                  "MaterialTypeIdentifierScreening")
  expect_s3_class(material[["materialTypeIdentifier"]], "json_class")
  expect_is(material[["materialCode"]], "integer")

  expect_identical(material, material_id(2475L))

  materials <- material_id(1:3, type = "gene")
  expect_s3_class(materials, "MaterialIdentifierScreening")
  expect_s3_class(materials, "json_vec")
  expect_length(materials, 3L)
  for (i in seq_along(materials)) {
    expect_s3_class(materials[[i]], "MaterialIdentifierScreening")
    expect_s3_class(materials[[i]], "json_class")
    expect_true(has_fields(materials[[i]],
                           c("materialTypeIdentifier", "materialCode")))
    expect_s3_class(materials[[i]][["materialTypeIdentifier"]],
                    "MaterialTypeIdentifierScreening")
    expect_s3_class(materials[[i]][["materialTypeIdentifier"]], "json_class")
    expect_is(materials[[i]][["materialCode"]], "integer")
  }

  expect_identical(materials,
                   material_id(1:3, type = c("gene", "gene", "gene")))
  
  expect_error(material_id(1:3, type = c("gene", "gene")))
  expect_error(material_id(1:3, type = 1:3))
  expect_error(material_id(1:3, type = "gen"))

  material <- material_id(2475L, mode = "generic")
  expect_s3_class(material, "MaterialIdentifierGeneric")
  expect_s3_class(material, "json_class")
  expect_true(has_fields(material,
                         c("materialTypeIdentifier", "materialCode")))
  expect_s3_class(material[["materialTypeIdentifier"]],
                  "MaterialTypeIdentifierGeneric")
  expect_s3_class(material[["materialTypeIdentifier"]], "json_class")
  expect_is(material[["materialCode"]], "integer")
})

test_that("material types can be listed", {

  types_scr <- list_material_types()
  expect_s3_class(types_scr, "MaterialTypeIdentifierScreening")
  expect_s3_class(types_scr, "json_vec")
  expect_gte(length(types_scr), 1L)
  for (i in seq_along(types_scr)) {
    expect_s3_class(types_scr[[i]], "MaterialTypeIdentifierScreening")
    expect_s3_class(types_scr[[i]], "json_class")
    expect_true(has_fields(types_scr[[i]], "materialTypeCode"))
    expect_is(types_scr[[i]][["materialTypeCode"]], "character")
  }

  expect_identical(types_scr, list_material_types(mode = "screening"))

  types_gen <- list_material_types(mode = "generic")
  expect_s3_class(types_gen, "MaterialTypeIdentifierGeneric")
  expect_s3_class(types_gen, "json_vec")
  expect_gte(length(types_gen), 1L)
  for (i in seq_along(types_gen)) {
    expect_s3_class(types_gen[[i]], "MaterialTypeIdentifierGeneric")
    expect_s3_class(types_gen[[i]], "json_class")
    expect_true(has_fields(types_gen[[i]], "materialTypeCode"))
    expect_is(types_gen[[i]][["materialTypeCode"]], "character")
  }

  types_comp <- list_material_types(types = "compound")
  expect_s3_class(types_comp, "MaterialTypeIdentifierScreening")
  expect_s3_class(types_comp, "json_class")
  expect_true(has_fields(types_comp, "materialTypeCode"))
  expect_is(types_comp[["materialTypeCode"]], "character")

  types_cg <- list_material_types(types = c("compound", "gene"))
  expect_s3_class(types_cg, "MaterialTypeIdentifierScreening")
  expect_s3_class(types_cg, "json_vec")
  expect_gte(length(types_cg), 2L)
  for (i in seq_along(types_cg)) {
    expect_s3_class(types_cg[[i]], "MaterialTypeIdentifierScreening")
    expect_s3_class(types_cg[[i]], "json_class")
    expect_true(has_fields(types_cg[[i]], "materialTypeCode"))
    expect_is(types_cg[[i]][["materialTypeCode"]], "character")
  }

  expect_error(list_material_types(types = "foo"))
  expect_error(list_material_types(types = c("foo", "gene")))
})

test_that("materials can be listed", {

  check_skip()

  materials <- material_id(c(2475L, 3832L), mode = "generic")

  mat_1 <- list_material(tok, materials[[1]])
  expect_s3_class(mat_1, "MaterialGeneric")
  expect_s3_class(mat_1, "json_class")
  expect_true(has_fields(mat_1, "materialTypeIdentifier"))
  expect_identical(mat_1[["materialTypeIdentifier"]],
                   materials[[1]][["materialTypeIdentifier"]])

  mat_2 <- list_material(tok, materials)
  expect_s3_class(mat_2, "MaterialGeneric")
  expect_s3_class(mat_2, "json_vec")
  expect_length(mat_2, 2L)
  for (i in seq_along(mat_2)) {
    expect_s3_class(mat_2[[i]], "MaterialGeneric")
    expect_s3_class(mat_2[[i]], "json_class")
    expect_true(has_fields(mat_2[[i]], "materialTypeIdentifier"))
    expect_identical(mat_2[[i]][["materialTypeIdentifier"]],
                     materials[[i]][["materialTypeIdentifier"]])
  }

  material <- material_id("AMBION_S602", type = "sirna", mode = "generic")

  mat_1 <- list_material(tok, material)
  expect_s3_class(mat_1, "MaterialGeneric")
  expect_s3_class(mat_1, "json_class")
  expect_true(has_fields(mat_1, "materialTypeIdentifier"))
  expect_identical(mat_1[["materialTypeIdentifier"]],
                   material[["materialTypeIdentifier"]])

  plate_ids <- as_plate_id(plates[1:2])

  mat_1 <- list_material(tok, plates[[2]])
  expect_s3_class(mat_1, "PlateWellMaterialMapping")
  expect_s3_class(mat_1, "json_class")
  expect_true(has_fields(mat_1, "mapping"))
  expect_length(mat_1[["mapping"]], 384L)

  mat_2 <- list_material(tok, plates[1:2])
  expect_s3_class(mat_2, "PlateWellMaterialMapping")
  expect_s3_class(mat_2, "json_vec")
  expect_length(mat_2, 2L)
  for (i in seq_along(mat_2)) {
    expect_s3_class(mat_2[[i]], "PlateWellMaterialMapping")
    expect_s3_class(mat_2[[i]], "json_class")
    expect_true(has_fields(mat_2[[i]], "mapping"))
    expect_length(mat_2[[i]][["mapping"]], 384L)
  }

  expect_identical(list_material(tok, plate_ids[[2]]), mat_1)
  expect_identical(list_material(tok, plate_ids[1:2]), mat_2)

  expect_identical(list_material(tok, plate_meta[[2]]), mat_1)
  expect_identical(list_material(tok, plate_meta[1:2]), mat_2)

  types <- list_material_types()

  mat_11 <- list_material(tok, plates[[2]], types[[2]])
  expect_s3_class(mat_11, "PlateWellMaterialMapping")
  expect_s3_class(mat_11, "json_class")
  expect_true(has_fields(mat_11, "mapping"))
  expect_length(mat_11[["mapping"]], 384L)

  mat_12 <- list_material(tok, plates[[2]], types[1:2])
  expect_s3_class(mat_12, "PlateWellMaterialMapping")
  expect_s3_class(mat_12, "json_vec")
  expect_length(mat_12, 2L)
  for (i in seq_along(mat_12)) {
    expect_s3_class(mat_12[[i]], "PlateWellMaterialMapping")
    expect_s3_class(mat_12[[i]], "json_class")
    expect_true(has_fields(mat_12[[i]], "mapping"))
    expect_length(mat_12[[i]][["mapping"]], 384L)
  }

  mat_21 <- list_material(tok, plates[1:2], types[[2]])
  expect_s3_class(mat_21, "PlateWellMaterialMapping")
  expect_s3_class(mat_21, "json_vec")
  expect_length(mat_21, 2L)
  for (i in seq_along(mat_21)) {
    expect_s3_class(mat_21[[i]], "PlateWellMaterialMapping")
    expect_s3_class(mat_21[[i]], "json_class")
    expect_true(has_fields(mat_21[[i]], "mapping"))
    expect_length(mat_21[[i]][["mapping"]], 384L)
  }

  mat_22 <- list_material(tok, plates[1:2], types[1:2])
  expect_s3_class(mat_22, "PlateWellMaterialMapping")
  expect_s3_class(mat_22, "json_vec")
  expect_length(mat_22, 4L)
  for (i in seq_along(mat_22)) {
    expect_s3_class(mat_22[[i]], "PlateWellMaterialMapping")
    expect_s3_class(mat_22[[i]], "json_class")
    expect_true(has_fields(mat_22[[i]], "mapping"))
    expect_length(mat_22[[i]][["mapping"]], 384L)
  }

  mat_f_14 <- extract_well_material(mat_11, "F", 14)
  expect_s3_class(mat_f_14, "MaterialIdentifierScreening")
  expect_s3_class(mat_f_14, "json_class")
  expect_identical(mat_f_14, mat_22[[i]][["mapping"]][[134L]])

  mat_a_24 <- extract_well_material(mat_11, 1, 24)
  expect_is(mat_a_24, "list")
  expect_identical(mat_a_24, list())
  expect_identical(mat_a_24, mat_22[[i]][["mapping"]][[24L]])
})

test_that("materials can be converted", {

  gen_mat <- material_id(c(2475L, 3832L), mode = "generic")

  scr_mat_1 <- as_screening_mat_id(gen_mat[[1]])
  expect_s3_class(scr_mat_1, "MaterialIdentifierScreening")
  expect_s3_class(scr_mat_1, "json_class")
  expect_true(has_fields(scr_mat_1,
                         c("materialTypeIdentifier", "materialCode")))
  expect_s3_class(scr_mat_1[["materialTypeIdentifier"]],
                  "MaterialTypeIdentifierScreening")
  expect_s3_class(scr_mat_1[["materialTypeIdentifier"]], "json_class")
  expect_is(scr_mat_1[["materialCode"]], "integer")

  scr_mat_2 <- as_screening_mat_id(gen_mat)
  expect_s3_class(scr_mat_2, "MaterialIdentifierScreening")
  expect_s3_class(scr_mat_2, "json_vec")
  expect_length(scr_mat_2, 2L)
  for (i in seq_along(scr_mat_2)) {
    expect_s3_class(scr_mat_2[[i]], "MaterialIdentifierScreening")
    expect_s3_class(scr_mat_2[[i]], "json_class")
    expect_true(has_fields(scr_mat_2[[i]],
                           c("materialTypeIdentifier", "materialCode")))
    expect_s3_class(scr_mat_2[[i]][["materialTypeIdentifier"]],
                    "MaterialTypeIdentifierScreening")
    expect_s3_class(scr_mat_2[[i]][["materialTypeIdentifier"]], "json_class")
    expect_is(scr_mat_2[[i]][["materialCode"]], "integer")
  }

  expect_identical(scr_mat_1, as_screening_mat_id(scr_mat_1))
  expect_identical(scr_mat_2, as_screening_mat_id(scr_mat_2))

  scr_mat <- material_id(c(2475L, 3832L), mode = "screening")

  gen_mat_1 <- as_generic_mat_id(scr_mat[[1]])
  expect_s3_class(gen_mat_1, "MaterialIdentifierGeneric")
  expect_s3_class(gen_mat_1, "json_class")
  expect_true(has_fields(gen_mat_1,
                         c("materialTypeIdentifier", "materialCode")))
  expect_s3_class(gen_mat_1[["materialTypeIdentifier"]],
                  "MaterialTypeIdentifierGeneric")
  expect_s3_class(gen_mat_1[["materialTypeIdentifier"]], "json_class")
  expect_is(gen_mat_1[["materialCode"]], "integer")

  gen_mat_2 <- as_generic_mat_id(scr_mat)
  expect_s3_class(gen_mat_2, "MaterialIdentifierGeneric")
  expect_s3_class(gen_mat_2, "json_vec")
  expect_length(gen_mat_2, 2L)
  for (i in seq_along(gen_mat_2)) {
    expect_s3_class(gen_mat_2[[i]], "MaterialIdentifierGeneric")
    expect_s3_class(gen_mat_2[[i]], "json_class")
    expect_true(has_fields(gen_mat_2[[i]],
                           c("materialTypeIdentifier", "materialCode")))
    expect_s3_class(gen_mat_2[[i]][["materialTypeIdentifier"]],
                    "MaterialTypeIdentifierGeneric")
    expect_s3_class(gen_mat_2[[i]][["materialTypeIdentifier"]], "json_class")
    expect_is(gen_mat_2[[i]][["materialCode"]], "integer")
  }

  expect_identical(gen_mat_1, as_generic_mat_id(gen_mat_1))
  expect_identical(gen_mat_2, as_generic_mat_id(gen_mat_2))
})
