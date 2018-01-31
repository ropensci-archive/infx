context("list materials")

test_that("material ids can be created", {
  material <- material_id(2475L, type = "gene")
  expect_is(material, "MaterialIdentifierGeneric")
  expect_is(material, "json_vec")
  expect_true(has_subclass(material, "MaterialIdentifierGeneric"))
  expect_true(all(sapply(material, has_subclass,
              "MaterialIdentifierGeneric")))
  expect_equal(length(material), 1L)
  expect_identical(material_id(2475L), material_id(2475L, type = "gene"))

  materials <- material_id(1:3, type = c("gene", "gene", "gene"))
  expect_is(materials, "MaterialIdentifierGeneric")
  expect_is(materials, "json_vec")
  expect_true(has_subclass(materials, "MaterialIdentifierGeneric"))
  expect_true(all(sapply(materials, has_subclass,
              "MaterialIdentifierGeneric")))
  expect_equal(length(materials), 3L)

  expect_error(material_id(1:3, type = c("gene", "gene")))
  expect_error(material_id(1:3, type = 1:3))
  expect_error(material_id(1:3, type = "gen"))

  material <- material_id(2475L, mode = "screening")
  expect_is(material, "MaterialIdentifierScreening")
  expect_is(material, "json_vec")
  expect_true(has_subclass(material, "MaterialIdentifierScreening"))
  expect_true(all(sapply(material, has_subclass,
              "MaterialIdentifierScreening")))
  expect_equal(length(material), 1L)
})

test_that("materials can be listed", {
  materials <- material_id(c(2475L, 3832L))

  mat_1 <- list_material(tok, materials[[1]])
  expect_is(mat_1, "MaterialGeneric")
  expect_is(mat_1, "json_vec")
  expect_true(has_subclass(mat_1, "MaterialGeneric"))
  expect_true(all(sapply(mat_1, has_subclass, "MaterialGeneric")))
  expect_equal(length(mat_1), 1L)

  mat_2 <- list_material(tok, materials)
  expect_is(mat_2, "MaterialGeneric")
  expect_is(mat_2, "json_vec")
  expect_true(has_subclass(mat_2, "MaterialGeneric"))
  expect_true(all(sapply(mat_2, has_subclass, "MaterialGeneric")))
  expect_equal(length(mat_2), 2L)


  exp_ids <- list_experiment_ids(tok)
  plates <- list_plates(tok, exp_ids[[1]])
  plate_ids <- plate_to_plateid(plates[1:2])
  plate_meta <- list_plate_metadata(tok, plates[1:2])

  mat_1 <- list_material(tok, plates[[2]])
  expect_is(mat_1, "PlateWellMaterialMapping")
  expect_is(mat_1, "json_vec")
  expect_true(has_subclass(mat_1, "PlateWellMaterialMapping"))
  expect_true(all(sapply(mat_1, has_subclass, "PlateWellMaterialMapping")))
  expect_equal(length(mat_1), 1L)

  mat_2 <- list_material(tok, plates[1:2])
  expect_is(mat_2, "PlateWellMaterialMapping")
  expect_is(mat_2, "json_vec")
  expect_true(has_subclass(mat_2, "PlateWellMaterialMapping"))
  expect_true(all(sapply(mat_2, has_subclass, "PlateWellMaterialMapping")))
  expect_equal(length(mat_2), 2L)

  expect_identical(list_material(tok, plate_ids[[2]]), mat_1)
  expect_identical(list_material(tok, plate_ids[1:2]), mat_2)

  expect_identical(list_material(tok, plate_meta[[2]]), mat_1)
  expect_identical(list_material(tok, plate_meta[1:2]), mat_2)

  genes <- json_class(materialTypeCode = "GENE",
                      class = "MaterialTypeIdentifierScreening")
  map_gen <- list_material(tok, plates[[2]], genes)

  expect_true(all(sapply(mat_1[[1]][["mapping"]], is.null)))
  expect_false(all(sapply(map_gen[[1]][["mapping"]], is.null)))
})
