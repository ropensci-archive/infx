context("infx")

test_that("openbis api urls and docs links can be generated", {
  url <- api_url()
  expect_is(url, "character")
  expect_length(url, 1L)
  expect_match(url, "^https://")
  expect_identical(url, api_url("gis"))

  link <- docs_link()
  expect_is(link, "character")
  expect_length(link, 1L)
  expect_match(link, "^\\\\href\\{.+\\}\\{.+\\}$")
  expect_identical(link, docs_link("gis"))

  link <- docs_link(method_name = "foo")
  expect_is(link, "character")
  expect_length(link, 1L)
  expect_match(link, "^\\\\href\\{.+\\}\\{.+\\:foo}$")
  expect_identical(link, docs_link("gis", method_name = "foo"))
})
