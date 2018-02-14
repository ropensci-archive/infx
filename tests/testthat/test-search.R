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
})