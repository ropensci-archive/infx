context("list materials")

test_that("material ids can be created", {

  material <- material_id(2475L, type = "gene")
  expect_is(material, "MaterialIdentifierScreening")
  expect_is(material, "json_vec")
  expect_true(has_subclass(material, "MaterialIdentifierScreening"))
  expect_length(material, 1L)
  expect_true(has_subclass(material[[1L]], "MaterialIdentifierScreening"))
  expect_true(has_subclass(material[[1L]][["materialTypeIdentifier"]],
                           "MaterialTypeIdentifierScreening"))

  expect_identical(material, material_id(2475L))

  materials <- material_id(1:3, type = "gene")
  expect_is(materials, "MaterialIdentifierScreening")
  expect_is(materials, "json_vec")
  expect_true(has_subclass(materials, "MaterialIdentifierScreening"))
  expect_length(materials, 3L)
  for (i in seq_along(materials)) {
    expect_true(has_subclass(materials[[i]], "MaterialIdentifierScreening"))
    expect_true(has_subclass(materials[[i]][["materialTypeIdentifier"]],
                             "MaterialTypeIdentifierScreening"))
  }

  expect_identical(materials,
                   material_id(1:3, type = c("gene", "gene", "gene")))
  
  expect_error(material_id(1:3, type = c("gene", "gene")))
  expect_error(material_id(1:3, type = 1:3))
  expect_error(material_id(1:3, type = "gen"))

  material <- material_id(2475L, mode = "generic")
  expect_is(material, "MaterialIdentifierGeneric")
  expect_is(material, "json_vec")
  expect_true(has_subclass(material, "MaterialIdentifierGeneric"))
  expect_length(material, 1L)
  expect_true(has_subclass(material[[1L]], "MaterialIdentifierGeneric"))
  expect_true(has_subclass(material[[1L]][["materialTypeIdentifier"]],
                           "MaterialTypeIdentifierGeneric"))
})

test_that("material types can be listed", {

  types_scr <- list_material_types()
  expect_is(types_scr, "MaterialTypeIdentifierScreening")
  expect_is(types_scr, "json_vec")
  expect_true(has_subclass(types_scr, "MaterialTypeIdentifierScreening"))
  expect_gte(length(types_scr), 1L)
  for (i in seq_along(types_scr)) {
    expect_is(types_scr[[i]], "MaterialTypeIdentifierScreening")
    expect_is(types_scr[[i]], "json_class")
    expect_true(has_subclass(types_scr[[i]],
                             "MaterialTypeIdentifierScreening"))
  }

  expect_identical(types_scr, list_material_types(mode = "screening"))

  types_gen <- list_material_types(mode = "generic")
  expect_is(types_gen, "MaterialTypeIdentifierGeneric")
  expect_is(types_gen, "json_vec")
  expect_true(has_subclass(types_gen, "MaterialTypeIdentifierGeneric"))
  expect_gte(length(types_gen), 1L)
  for (i in seq_along(types_gen)) {
    expect_is(types_gen[[i]], "MaterialTypeIdentifierGeneric")
    expect_is(types_gen[[i]], "json_class")
    expect_true(has_subclass(types_gen[[i]],
                             "MaterialTypeIdentifierGeneric"))
  }

  types_comp <- list_material_types(types = "compound")
  expect_is(types_comp, "MaterialTypeIdentifierScreening")
  expect_is(types_comp, "json_vec")
  expect_true(has_subclass(types_comp, "MaterialTypeIdentifierScreening"))
  expect_length(types_comp, 1L)
  expect_is(types_comp[[1L]], "MaterialTypeIdentifierScreening")
  expect_is(types_comp[[1L]], "json_class")
  expect_true(has_subclass(types_comp[[1L]],
                           "MaterialTypeIdentifierScreening"))

  types_cg <- list_material_types(types = c("compound", "gene"))
  expect_is(types_cg, "MaterialTypeIdentifierScreening")
  expect_is(types_cg, "json_vec")
  expect_true(has_subclass(types_cg, "MaterialTypeIdentifierScreening"))
  expect_gte(length(types_cg), 2L)
  for (i in seq_along(types_cg)) {
    expect_is(types_cg[[i]], "MaterialTypeIdentifierScreening")
    expect_is(types_cg[[i]], "json_class")
    expect_true(has_subclass(types_cg[[i]],
                             "MaterialTypeIdentifierScreening"))
  }

  expect_error(list_material_types(types = "foo"))
  expect_error(list_material_types(types = c("foo", "gene")))
})

test_that("materials can be listed", {

  check_skip()

  materials <- material_id(c(2475L, 3832L), mode = "generic")

  mat_1 <- list_material(tok, materials[[1]])
  expect_is(mat_1, "MaterialGeneric")
  expect_is(mat_1, "json_vec")
  expect_true(has_subclass(mat_1, "MaterialGeneric"))
  expect_length(mat_1, 1L)
  expect_true(has_subclass(mat_1[[1L]], "MaterialGeneric"))

  mat_2 <- list_material(tok, materials)
  expect_is(mat_2, "MaterialGeneric")
  expect_is(mat_2, "json_vec")
  expect_true(has_subclass(mat_2, "MaterialGeneric"))
  expect_length(mat_2, 2L)
  for (i in seq_along(mat_2))
    expect_true(has_subclass(mat_2[[i]], "MaterialGeneric"))

  material <- material_id("AMBION_S602", type = "sirna", mode = "generic")

  mat_1 <- list_material(tok, material)
  expect_is(mat_1, "MaterialGeneric")
  expect_is(mat_1, "json_vec")
  expect_true(has_subclass(mat_1, "MaterialGeneric"))
  expect_length(mat_1, 1L)
  expect_true(has_subclass(mat_1[[1L]], "MaterialGeneric"))

  plate_ids <- as_plateid(plates[1:2])

  mat_1 <- list_material(tok, plates[[2]])
  expect_is(mat_1, "PlateWellMaterialMapping")
  expect_is(mat_1, "json_vec")
  expect_true(has_subclass(mat_1, "PlateWellMaterialMapping"))
  expect_length(mat_1, 1L)
  expect_true(has_subclass(mat_1[[1L]], "PlateWellMaterialMapping"))

  mat_2 <- list_material(tok, plates[1:2])
  expect_is(mat_2, "PlateWellMaterialMapping")
  expect_is(mat_2, "json_vec")
  expect_true(has_subclass(mat_2, "PlateWellMaterialMapping"))
  expect_length(mat_2, 2L)
  for (i in seq_along(mat_2))
    expect_true(has_subclass(mat_2[[i]], "PlateWellMaterialMapping"))

  expect_identical(list_material(tok, plate_ids[[2]]), mat_1)
  expect_identical(list_material(tok, plate_ids[1:2]), mat_2)

  expect_identical(list_material(tok, plate_meta[[2]]), mat_1)
  expect_identical(list_material(tok, plate_meta[1:2]), mat_2)

  types <- list_material_types()

  mat_11 <- list_material(tok, plates[[2]], types[[2]])
  expect_is(mat_11, "PlateWellMaterialMapping")
  expect_is(mat_11, "json_vec")
  expect_true(has_subclass(mat_11, "PlateWellMaterialMapping"))
  expect_length(mat_11, 1L)
  expect_true(has_subclass(mat_11[[1L]], "PlateWellMaterialMapping"))

  mat_12 <- list_material(tok, plates[[2]], types[1:2])
  expect_is(mat_12, "PlateWellMaterialMapping")
  expect_is(mat_12, "json_vec")
  expect_true(has_subclass(mat_12, "PlateWellMaterialMapping"))
  expect_length(mat_12, 2L)
  for (i in seq_along(mat_12))
    expect_true(has_subclass(mat_12[[i]], "PlateWellMaterialMapping"))

  mat_21 <- list_material(tok, plates[1:2], types[[2]])
  expect_is(mat_21, "PlateWellMaterialMapping")
  expect_is(mat_21, "json_vec")
  expect_true(has_subclass(mat_21, "PlateWellMaterialMapping"))
  expect_length(mat_21, 2L)
  for (i in seq_along(mat_21))
    expect_true(has_subclass(mat_21[[i]], "PlateWellMaterialMapping"))

  mat_22 <- list_material(tok, plates[1:2], types[1:2])
  expect_is(mat_22, "PlateWellMaterialMapping")
  expect_is(mat_22, "json_vec")
  expect_true(has_subclass(mat_22, "PlateWellMaterialMapping"))
  expect_length(mat_22, 4L)
  for (i in seq_along(mat_22))
    expect_true(has_subclass(mat_22[[i]], "PlateWellMaterialMapping"))

  mat_f_14 <- extract_well_material(mat_11, "F", 14)
  expect_is(mat_f_14, "MaterialIdentifierScreening")
  expect_is(mat_f_14, "json_vec")
  expect_true(has_subclass(mat_f_14, "MaterialIdentifierScreening"))
  expect_length(mat_f_14, 1L)
  expect_true(has_subclass(mat_f_14[[1L]], "MaterialIdentifierScreening"))
})

test_that("materials can be converted", {

  gen_mat <- material_id(c(2475L, 3832L), mode = "generic")

  scr_mat_1 <- as_screening_mat_id(gen_mat[[1]])
  expect_is(scr_mat_1, "MaterialIdentifierScreening")
  expect_is(scr_mat_1, "json_vec")
  expect_true(has_subclass(scr_mat_1, "MaterialIdentifierScreening"))
  expect_length(scr_mat_1, 1L)
  expect_true(has_subclass(scr_mat_1[[1L]], "MaterialIdentifierScreening"))

  scr_mat_2 <- as_screening_mat_id(gen_mat)
  expect_is(scr_mat_2, "MaterialIdentifierScreening")
  expect_is(scr_mat_2, "json_vec")
  expect_true(has_subclass(scr_mat_2, "MaterialIdentifierScreening"))
  expect_length(scr_mat_2, 2L)
  for (i in seq_along(scr_mat_2))
    expect_true(has_subclass(scr_mat_2[[i]], "MaterialIdentifierScreening"))

  expect_identical(scr_mat_1, as_screening_mat_id(scr_mat_1))
  expect_identical(scr_mat_2, as_screening_mat_id(scr_mat_2))

  scr_mat <- material_id(c(2475L, 3832L), mode = "screening")

  gen_mat_1 <- as_generic_mat_id(scr_mat[[1]])
  expect_is(gen_mat_1, "MaterialIdentifierGeneric")
  expect_is(gen_mat_1, "json_vec")
  expect_true(has_subclass(gen_mat_1, "MaterialIdentifierGeneric"))
  expect_length(gen_mat_1, 1L)
  expect_true(has_subclass(gen_mat_1[[1L]], "MaterialIdentifierGeneric"))

  gen_mat_2 <- as_generic_mat_id(scr_mat)
  expect_is(gen_mat_2, "MaterialIdentifierGeneric")
  expect_is(gen_mat_2, "json_vec")
  expect_true(has_subclass(gen_mat_2, "MaterialIdentifierGeneric"))
  expect_length(gen_mat_2, 2L)
  for (i in seq_along(gen_mat_2))
    expect_true(has_subclass(gen_mat_2[[i]], "MaterialIdentifierGeneric"))

  expect_identical(gen_mat_1, as_generic_mat_id(gen_mat_1))
  expect_identical(gen_mat_2, as_generic_mat_id(gen_mat_2))
})
