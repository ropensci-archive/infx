context("search")

test_that("search for datasets", {

  check_skip()

  sc <- search_criteria(attribute_clause("20111223114629018-318033",
                                         "perm_id"))

  ds_1 <- search_openbis(tok, sc)

  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "DataSet"))

  sc <- search_criteria(time_attribute_clause(as.Date("2011-12-23")))

  ds_n <- search_openbis(tok, sc)

  expect_is(ds_n, "DataSet")
  expect_is(ds_n, "json_vec")
  expect_identical(get_subclass(ds_n), "DataSet")
  expect_true(all(sapply(ds_n, has_subclass, "DataSet")))
  expect_gte(length(ds_n), 1L)

  sc <- search_criteria(property_clause("20715", "IBRAIN2.DATASET.ID"))

  ds_1 <- search_openbis(tok, sc)

  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "DataSet"))

  sc <- search_criteria(any_property_clause("20715"))

  ds_n <- search_openbis(tok, sc)

  expect_is(ds_n, "DataSet")
  expect_is(ds_n, "json_vec")
  expect_identical(get_subclass(ds_n), "DataSet")
  expect_true(all(sapply(ds_n, has_subclass, "DataSet")))
  expect_gte(length(ds_n), 1L)

  sc <- search_criteria(any_field_clause("20111223114629018-318033"))

  ds_1 <- search_openbis(tok, sc)

  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "DataSet"))
})

test_that("search for samples", {

  check_skip()

  sc <- search_criteria(attribute_clause("20121012090101856-1360590",
                                         "perm_id"))

  ds_1 <- search_openbis(tok, sc, "sample")
  expect_is(ds_1, "Sample")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "Sample")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "Sample"))

  ds_1 <- search_openbis(tok, sc, "sample", "properties")
  expect_is(ds_1, "Sample")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "Sample")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "Sample"))
})

test_that("search with sub-criteria", {

  check_skip()

  exp <- search_sub_criteria(
    search_criteria(attribute_clause("ADENO-AU-K1", "code")),
    "experiment"
  )

  sc <- search_criteria(attribute_clause("PLATE", "type"),
                        sub_criteria = exp)

  ds_n <- search_openbis(tok, sc, "sample")

  expect_is(ds_n, "Sample")
  expect_is(ds_n, "json_vec")
  expect_gte(length(ds_n), 1L)
  for (i in seq_along(ds_n)) {
    expect_is(ds_n[[i]], "Sample")
    expect_is(ds_n[[i]], "json_class")
    expect_match(ds_n[[i]][["experimentIdentifierOrNull"]], "ADENO-AU-K1")
  }
})

test_that("search for material", {

  check_skip()

  mat_1 <- search_openbis(tok,
    search_criteria(property_clause("MTOR", "GENE_SYMBOL")), "material")
  expect_is(mat_1, "MaterialGeneric")
  expect_is(mat_1, "json_vec")
  expect_equal(length(mat_1), 1L)
  expect_is(mat_1[[1]], "MaterialGeneric")
  expect_is(mat_1[[1]], "json_class")

  expect_identical(
    search_openbis(tok, search_criteria(attribute_clause("2475")), "material"),
    mat_1)
  expect_identical(
    search_openbis(tok, search_criteria(attribute_clause(2475)), "material"),
    mat_1)
})
