context("utility functions")

test_that("the default error works", {
  expect_error(error_default(1L), "cannot handle objects of type")
  expect_error(error_default(structure(1L, class = "foo")), "foo")
  expect_error(error_default(structure(1L, class = c("foo", "bar"))), "bar")
  expect_error(error_default(1L, prefix = "foo"), "foo")
})

test_that("property types can be listed", {
  pt <- list_property_types(tok)
  expect_is(pt, "list")
  expect_equal(length(pt), 2L)
  expect_true(all(sapply(pt, is_json_vec)))
  expect_true(all(sapply(pt, get_subclass) %in%
                c("ControlledVocabularyPropertyType", "PropertyType")))
})
