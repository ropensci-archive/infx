context("project")

test_that("projects can be listed", {
  expect_is(projects, "Project")
  expect_is(projects, "json_vec")
  expect_identical(get_subclass(projects), "Project")
  expect_true(all(sapply(projects, has_subclass, "Project")))
  expect_gte(length(projects), 1L)
})
