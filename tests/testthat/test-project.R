context("project")

test_that("projects can be listed", {
  proj <- list_projects(tok)
  expect_is(proj, "Project")
  expect_is(proj, "json_vec")
  expect_identical(get_subclass(proj), "Project")
  expect_true(all(sapply(proj, has_subclass, "Project")))
  expect_gte(length(proj), 1L)
})
