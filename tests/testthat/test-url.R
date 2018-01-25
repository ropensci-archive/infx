context("urls can be determined")

test_that("data store servers can be listed", {
  dss <- list_datastores(tok)
  expect_is(dss, "DataStore")
  expect_is(dss, "json_vec")
  expect_identical(get_common_subclass(dss), "DataStore")
  expect_true(all(sapply(dss, has_json_subclass, "DataStore")))
  expect_gte(length(dss), 1L)
})
