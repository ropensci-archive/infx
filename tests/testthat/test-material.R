context("list materials")

test_that("material ids can be created", {
  material <- material_id(2475L, type = "gene")
  expect_is(material, "MaterialIdentifierGeneric")
  expect_is(material, "json_vec")
  expect_identical(get_common_subclass(material), "MaterialIdentifierGeneric")
  expect_true(all(sapply(material, has_json_subclass,
              "MaterialIdentifierGeneric")))
  expect_equal(length(material), 1L)
  expect_identical(material_id(2475L), material_id(2475L, type = "gene"))

  materials <- material_id(1:3, type = c("gene", "gene", "gene"))
  expect_is(materials, "MaterialIdentifierGeneric")
  expect_is(materials, "json_vec")
  expect_identical(get_common_subclass(materials), "MaterialIdentifierGeneric")
  expect_true(all(sapply(materials, has_json_subclass,
              "MaterialIdentifierGeneric")))
  expect_equal(length(materials), 3L)

  expect_error(material_id(1:3, type = c("gene", "gene")))
  expect_error(material_id(1:3, type = 1:3))
  expect_error(material_id(1:3, type = "gen"))
})

test_that("materials can be listed", {
  materials <- material_id(c(2475L, 3832L))

  mat_1 <- list_material(tok, materials[[1]])
  expect_is(mat_1, "MaterialGeneric")
  expect_is(mat_1, "json_vec")
  expect_identical(get_common_subclass(mat_1), "MaterialGeneric")
  expect_true(all(sapply(mat_1, has_json_subclass,
              "MaterialGeneric")))
  expect_equal(length(mat_1), 1L)

  mat_2 <- list_material(tok, materials)
  expect_is(mat_2, "MaterialGeneric")
  expect_is(mat_2, "json_vec")
  expect_identical(get_common_subclass(mat_2), "MaterialGeneric")
  expect_true(all(sapply(mat_2, has_json_subclass,
              "MaterialGeneric")))
  expect_equal(length(mat_2), 2L)
})
