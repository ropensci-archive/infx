context("utility functions")

test_that("the default error works", {
  expect_error(error_default(1L),
               "cannot handle objects of type c(\"integer\").")
  expect_error(error_default(structure(1L, class = "foo")),
               "cannot handle objects of type c(\"foo\").")
  expect_error(error_default(structure(1L, class = c("foo", "bar"))),
               "cannot handle objects of type c(\"foo\", \"bar\").")
})
