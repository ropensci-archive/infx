context("project")

test_that("projects can be listed", {
  expect_s3_class(projects, "Project")
  expect_s3_class(projects, "json_vec")
  expect_gte(length(projects), 1L)
  for (i in seq_along(projects)) {
    expect_s3_class(projects[[i]], "Project")
    expect_s3_class(projects[[i]], "json_class")
  }
})
