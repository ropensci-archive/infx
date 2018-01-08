context("json object vectors")

test_that("common subclass can be determined", {
  expect_true(has_common_subclass(
    list(structure(list("a"), class = c("foo", "json_class")),
         structure(list("b"), class = c("foo", "json_class")))))
  expect_true(has_common_subclass(
    list(structure(list("a"), class = c("foo", "bar", "json_class")),
         structure(list("b"), class = c("foo", "bar", "json_class")))))
  expect_false(has_common_subclass(
    list(structure("a", class = c("foo", "json_class")),
         structure("b", class = c("bar", "json_class")))))
  expect_false(has_common_subclass(
    list(structure("a", class = c("foo")),
         structure("b", class = c("bar", "json_class")))))
  expect_true(has_common_subclass(
    list(structure("a", class = c("foo", "json_class")))))
  expect_false(has_common_subclass(
    list(structure("a", class = c("foo")))))
  expect_false(has_common_subclass(
    list(structure("a", class = c("json_class", "foo")))))
  expect_false(has_common_subclass(
    list(structure("a", class = c("json_class", "foo")),
         structure("b", class = c("foo", "json_class")))))
  expect_false(has_common_subclass(NULL))
  expect_false(has_common_subclass(NA))
  expect_false(has_common_subclass("a"))
  expect_error(has_common_subclass())

  expect_equal(get_common_subclass(
    list(structure("a", class = c("foo", "json_class")),
         structure("b", class = c("foo", "json_class")))), "foo")
  expect_equal(get_common_subclass(
    list(structure("a", class = c("foo", "json_class")))), "foo")
  expect_error(get_common_subclass(
    list(structure("a", class = c("foo", "json_class")),
         structure("b", class = c("bar", "json_class")))))
  expect_error(get_common_subclass(
    list(structure("a", class = "foo"), structure("b", class = "foo"))))
  expect_equal(get_common_subclass(
    list(structure("a", class = c("foo", "bar", "json_class")),
         structure("b", class = c("foo", "bar", "json_class")))),
    c("foo", "bar"))
  expect_error(get_common_subclass(
    list(structure("a", class = c("foo", "bar", "json_class")),
         structure("b", class = c("bar", "foo", "json_class")))))
})

test_that("json_vec objects can be created", {
  a <- structure("a", class = c("foo", "json_class"))
  b <- structure("b", class = c("foo", "json_class"))
  expect_s3_class(json_vec(a, b), c("foo", "json_vec"))
  expect_identical(json_vec(a, b), new_json_vec(list(a, b)))
})