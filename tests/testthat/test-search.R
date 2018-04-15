context("search")

test_that("search for datasets", {

  check_skip()

  perm_id <- "20111223114629018-318033"
  sc <- search_criteria(attribute_clause("perm_id", perm_id))

  ds_1 <- search_openbis(tok, sc)

  expect_s3_class(ds_1, "DataSet")
  expect_s3_class(ds_1, "json_vec")
  expect_length(ds_1, 1L)
  expect_s3_class(ds_1[[1]], "DataSet")
  expect_s3_class(ds_1[[1]], "json_class")
  expect_identical(get_field(ds_1[[1]], "code"), perm_id)

  date <- as.Date("2011-11-15")
  sc <- search_criteria(time_attribute_clause(value = date))

  ds_n <- search_openbis(tok, sc)

  expect_s3_class(ds_n, "DataSet")
  expect_s3_class(ds_n, "json_vec")
  expect_gte(length(ds_n), 1L)
  for (i in seq_along(ds_n)) {
    expect_s3_class(ds_n[[i]], "DataSet")
    expect_s3_class(ds_n[[i]], "json_class")
    expect_identical(
      as.Date(
        as.POSIXct(
          ds_n[[i]][["registrationDetails"]][["registrationDate"]] / 1000,
          origin = "1970-01-01"
        )
      ),
      date
    )
  }

  ds_id <- "20715"
  sc <- search_criteria(property_clause("IBRAIN2.DATASET.ID", ds_id))

  ds_1 <- search_openbis(tok, sc)

  expect_s3_class(ds_1, "DataSet")
  expect_s3_class(ds_1, "json_vec")
  expect_length(ds_1, 1L)
  expect_s3_class(ds_1[[1]], "DataSet")
  expect_s3_class(ds_1[[1]], "json_class")
  expect_identical(ds_1[[1]][["properties"]][["IBRAIN2.DATASET.ID"]], ds_id)

  sc <- search_criteria(any_property_clause(ds_id))

  ds_n <- search_openbis(tok, sc)

  expect_s3_class(ds_n, "DataSet")
  expect_s3_class(ds_n, "json_vec")
  expect_gte(length(ds_n), 1L)
  for (i in seq_along(ds_n)) {
    expect_s3_class(ds_n[[i]], "DataSet")
    expect_s3_class(ds_n[[i]], "json_class")
    expect_true(ds_id %in% ds_n[[i]][["properties"]])
  }

  sc <- search_criteria(any_field_clause(perm_id))

  ds_1 <- search_openbis(tok, sc)

  expect_s3_class(ds_1, "DataSet")
  expect_s3_class(ds_1, "json_vec")
  expect_length(ds_1, 1L)
  expect_s3_class(ds_1[[1]], "DataSet")
  expect_s3_class(ds_1[[1]], "json_class")
  expect_identical(get_field(ds_1[[1]], "code"), perm_id)
})

test_that("search for samples", {

  check_skip()

  perm_id <- "20121012090101856-1360590"
  sc <- search_criteria(attribute_clause("perm_id", perm_id))

  samp_1 <- search_openbis(tok, sc, "sample")
  expect_s3_class(samp_1, "Sample")
  expect_s3_class(samp_1, "json_vec")
  expect_length(samp_1, 1L)
  expect_s3_class(samp_1[[1]], "Sample")
  expect_s3_class(samp_1[[1]], "json_class")
  expect_identical(get_field(samp_1[[1]], "permId"), perm_id)
  expect_false("PARENTS" %in% samp_1[[1]][["retrievedFetchOptions"]])

  samp_1 <- search_openbis(tok, sc, "sample", "parents")
  expect_s3_class(samp_1, "Sample")
  expect_s3_class(samp_1, "json_vec")
  expect_length(samp_1, 1L)
  expect_s3_class(samp_1[[1]], "Sample")
  expect_s3_class(samp_1[[1]], "json_class")
  expect_identical(get_field(samp_1[[1]], "permId"), perm_id)
  expect_true("PARENTS" %in% samp_1[[1]][["retrievedFetchOptions"]])
})

test_that("search with sub-criteria", {

  check_skip()

  exp <- search_sub_criteria(
    search_criteria(attribute_clause("code", "ADENO-AU-K1")),
    "experiment"
  )

  sc <- search_criteria(attribute_clause("type", "PLATE"),
                        sub_criteria = exp)

  ds_n <- search_openbis(tok, sc, "sample")

  expect_s3_class(ds_n, "Sample")
  expect_s3_class(ds_n, "json_vec")
  expect_gte(length(ds_n), 1L)
  for (i in seq_along(ds_n)) {
    expect_s3_class(ds_n[[i]], "Sample")
    expect_s3_class(ds_n[[i]], "json_class")
    expect_match(get_field(ds_n[[i]], "experimentIdentifierOrNull"),
                 "ADENO-AU-K1")
  }
})

test_that("search for material", {

  check_skip()

  mat_1 <- search_openbis(tok,
    search_criteria(property_clause("GENE_SYMBOL", "MTOR")), "material")
  expect_s3_class(mat_1, "MaterialGeneric")
  expect_s3_class(mat_1, "json_vec")
  expect_equal(length(mat_1), 1L)
  expect_s3_class(mat_1[[1]], "MaterialGeneric")
  expect_s3_class(mat_1[[1]], "json_class")
  expect_identical(mat_1[[1]][["properties"]][["GENE_SYMBOL"]], "MTOR")

  expect_identical(
    search_openbis(tok, search_criteria(attribute_clause(value = "2475")),
                                        "material"),
    mat_1)
  expect_identical(
    search_openbis(tok, search_criteria(attribute_clause(value = 2475)),
                                        "material"),
    mat_1)
})

test_that("property types can be listed", {
  pt <- list_property_types(tok)
  expect_is(pt, "list")
  expect_length(pt, 2L)
  for (i in seq_along(pt))
    expect_s3_class(pt[[i]], "json_vec")
    for (j in seq_along(pt[[i]]))
      expect_s3_class(pt[[i]][[j]], "json_class")

  for (i in seq_along(pt[[1]]))
    expect_s3_class(pt[[1]][[i]], "ControlledVocabularyPropertyType")
  for (i in seq_along(pt[[2]]))
    expect_s3_class(pt[[2]][[i]], "PropertyType")
})
