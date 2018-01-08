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

  expect_error(json_vec(structure("a", class = c("foo")),
                        structure("b", class = c("foo"))))
  expect_error(json_vec(structure("a", class = c("foo", "json_class")),
                        structure("b", class = c("bar", "json_class"))))
  expect_error(json_vec(structure("a", class = c("json_class")),
                        structure("b", class = c("json_class"))))

  expect_identical(json_vec(a, b), as.json_vec(json_vec(a, b)))
  expect_identical(json_vec(a), as.json_vec(a))
  expect_identical(json_vec(a, b), as.json_vec(list(a, b)))
  expect_error(as.json_vec(list("a", "b")))
  expect_error(as.json_vec("a"))
  expect_identical(as.list(json_vec(a, b)), list(a, b))
})

test_that("json_vec helpers work", {
  expect_true(has_common_subclass(
    structure("a", class = c("foo", "json_class"))))
  expect_true(has_common_subclass(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "json_class")))))
  expect_false(has_common_subclass(list(
    structure("a", class = c("foo")),
    structure("b", class = c("foo")))))
  expect_false(has_common_subclass(
    structure("a", class = c("foo"))))
  expect_false(has_common_subclass(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("bar", "json_class")))))
  expect_false(has_common_subclass(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "bar", "json_class")))))

  expect_equal(get_common_subclass(
    structure("a", class = c("foo", "json_class"))), "foo")
  expect_equal(get_common_subclass(
    structure("a", class = c("foo", "bar", "json_class"))), c("foo", "bar"))
  expect_error(get_common_subclass("a"))
  expect_error(get_common_subclass(structure("a", class = c("foo"))))
  expect_error(get_common_subclass(structure("a", class = c("foo", "bar"))))
  expect_equal(get_common_subclass(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "json_class")))), "foo")
  expect_equal(get_common_subclass(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class")))), c("foo", "bar"))
  expect_error(get_common_subclass(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("bar", "foo", "json_class")))))
  expect_error(get_common_subclass(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("bar", "json_class")))))
  expect_error(get_common_subclass(list(
    structure("a", class = c("json_class")),
    structure("b", class = c("json_class")))))
  expect_error(get_common_subclass(list(
    structure("a", class = c("foo")),
    structure("b", class = c("foo")))))
  expect_equal(get_common_subclass(json_vec(
    structure("a", class = c("foo", "json_class")))), c("foo"))
  expect_equal(get_common_subclass(json_vec(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "json_class")))), c("foo"))
  expect_equal(get_common_subclass(json_vec(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class")))), c("foo", "bar"))
  expect_error(get_common_subclass(json_vec(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "json_class")))))
  expect_error(get_common_subclass(json_vec(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("bar", "foo", "json_class")))))
  expect_error(get_common_subclass(json_vec(
    structure("a", class = c("foo", "bar")),
    structure("b", class = c("foo", "bar", "json_class")))))
  expect_error(get_common_subclass(json_vec(
    structure("a", class = c("foo")),
    structure("b", class = c("foo")))))
  expect_error(get_common_subclass(json_vec(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("bar", "json_class")))))

  expect_error(get_common_subclass(c("a", "b")))

  expect_true(is.json_vec(json_vec(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "json_class")))))
  expect_true(is.json_vec(json_vec(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class")))))
  expect_true(is.json_vec(structure(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class"))),
    class = c("foo", "bar", "json_vec"))))
  expect_false(is.json_vec(structure(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class"))),
    class = c("foo", "bar"))))
  expect_false(is.json_vec(structure(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class"))),
    class = c("bar", "foo", "json_vec"))))
  expect_false(is.json_vec(structure(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class"))),
    class = c("foo", "json_vec", "bar"))))
  expect_false(is_json_vec(structure(list(
    structure("a", class = c("foo", "bar")),
    structure("b", class = c("foo", "bar", "json_class"))),
    class = c("foo", "bar", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("foo", "bar", "json_class"))),
    class = c("json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure("a", class = c("foo", "bar", "json_class")),
    structure("b", class = c("bar", "foo", "json_class"))),
    class = c("foo", "bar", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("bar", "json_class"))),
    class = c("foo", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "json_class"))),
    class = c("bar", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure("a", class = c("foo", "json_class")),
    structure("b", class = c("foo", "json_class"))),
    class = c("json_vec"))))
})
