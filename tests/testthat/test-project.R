context("project")

test_that("projects can be listed", {
  expect_is(projects, "Project")
  expect_is(projects, "json_vec")
  expect_gte(length(projects), 1L)
  for (i in seq_along(projects)) {
    expect_is(projects[[i]], "Project")
    expect_is(projects[[i]], "json_class")
  }
})
