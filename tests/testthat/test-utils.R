context("utility functions")

test_that("the default error works", {
  expect_error(error_default(1L), "cannot handle objects of type")
  expect_error(error_default(structure(1L, class = "foo")), "foo")
  expect_error(error_default(structure(1L, class = c("foo", "bar"))), "bar")
})
