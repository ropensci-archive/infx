context("search")

test_that("search for datasets", {

  check_skip()

  sc <- search_criteria(
    attribute_match_clause("20111223114629018-318033",
                           match_clause_attribute("perm_id")))

  ds_1 <- search_openbis(tok, sc, "data_set")

  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "DataSet"))

  sc <- search_criteria(
    time_attribute_match_clause(as.Date("2011-12-23"),
                                compare_mode = compare_mode("eq")))

  ds_n <- search_openbis(tok, sc, "data_set")

  expect_is(ds_n, "DataSet")
  expect_is(ds_n, "json_vec")
  expect_identical(get_subclass(ds_n), "DataSet")
  expect_true(all(sapply(ds_n, has_subclass, "DataSet")))
  expect_gte(length(ds_n), 1L)

  sc <- search_criteria(
    property_match_clause("20715", "IBRAIN2.DATASET.ID"))

  ds_1 <- search_openbis(tok, sc, "data_set")

  expect_is(ds_1, "DataSet")
  expect_is(ds_1, "json_vec")
  expect_identical(get_subclass(ds_1), "DataSet")
  expect_equal(length(ds_1), 1L)
  expect_true(has_subclass(ds_1[[1]], "DataSet"))
})