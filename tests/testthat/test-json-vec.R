context("json_vec objects")

test_that("common subclass can be determined", {
  expect_true(has_common_subclass(
    list(structure(list("a"), class = c("foo", "json_class")),
         structure(list("b"), class = c("foo", "json_class")))))
  expect_true(has_common_subclass(
    list(structure(list("a"), class = c("foo", "bar", "json_class")),
         structure(list("b"), class = c("foo", "bar", "json_class")))))
  expect_false(has_common_subclass(
    list(structure("a", class = c("foo", "json_class")),
         structure("b", class = c("foo", "json_class")))))
  expect_false(has_common_subclass(
    list(structure(list("a"), class = c("foo", "json_class")),
         structure(list("b"), class = c("bar", "json_class")))))
  expect_false(has_common_subclass(
    list(structure(list("a"), class = c("foo")),
         structure(list("b"), class = c("bar", "json_class")))))
  expect_true(has_common_subclass(
    list(structure(list("a"), class = c("foo", "json_class")))))
  expect_false(has_common_subclass(
    list(structure(list("a"), class = c("foo")))))
  expect_false(has_common_subclass(
    list(structure(list("a"), class = c("json_class", "foo")))))
  expect_false(has_common_subclass(
    list(structure(list("a"), class = c("json_class", "foo")),
         structure(list("b"), class = c("foo", "json_class")))))
  expect_false(has_common_subclass(NULL))
  expect_false(has_common_subclass(NA))
  expect_false(has_common_subclass("a"))
  expect_error(has_common_subclass())

  expect_equal(get_subclass(
    list(structure(list("a"), class = c("foo", "json_class")),
         structure(list("b"), class = c("foo", "json_class")))), "foo")
  expect_equal(get_subclass(
    list(structure(list("a"), class = c("foo", "json_class")))), "foo")
  expect_error(get_subclass(
    list(structure(list("a"), class = c("foo", "json_class")),
         structure(list("b"), class = c("bar", "json_class")))))
  expect_error(get_subclass(
    list(structure(list("a"), class = "foo"),
         structure(list("b"), class = "foo"))))
  expect_equal(get_subclass(
    list(structure(list("a"), class = c("foo", "bar", "json_class")),
         structure(list("b"), class = c("foo", "bar", "json_class")))),
    c("foo", "bar"))
  expect_error(get_subclass(
    list(structure(list("a"), class = c("foo", "bar", "json_class")),
         structure(list("b"), class = c("bar", "foo", "json_class")))))
})

test_that("json_vec objects can be created", {
  a <- structure(list("a"), class = c("foo", "json_class"))
  b <- structure(list("b"), class = c("foo", "json_class"))
  expect_s3_class(json_vec(a, b), c("foo", "json_vec"))
  expect_identical(json_vec(a, b), new_json_vec(list(a, b)))

  expect_error(json_vec(structure(list("a"), class = c("foo")),
                        structure(list("b"), class = c("foo"))))
  expect_error(json_vec(structure(list("a"), class = c("foo", "json_class")),
                        structure(list("b"), class = c("bar", "json_class"))))
  expect_error(json_vec(structure(list("a"), class = c("json_class")),
                        structure(list("b"), class = c("json_class"))))

  expect_identical(json_vec(a, b), as.json_vec(json_vec(a, b)))
  expect_identical(json_vec(a), as.json_vec(a))
  expect_identical(json_vec(a, b), as.json_vec(list(a, b)))
  expect_error(as.json_vec(list("a", "b")))
  expect_error(as.json_vec("a"))
  expect_identical(as.list(json_vec(a, b)), list(a, b))

  vec <- json_vec(a, b)
  expect_s3_class(vec[1], "json_vec")
  expect_s3_class(vec[[1]], "json_class")
  vec[2] <- a
  expect_identical(vec[[1]], vec[[2]])
  vec[1:2] <- list(b, a)
  expect_identical(vec[[1]], b)
  expect_identical(vec[[2]], a)
  expect_error(vec[[1]] <- "a")
  expect_s3_class(c(json_vec(a, b), json_vec(b, a)), "json_vec")

  expect_error(as_json_vec(as.list(letters[1:3])))
  expect_identical(as_json_vec(as.list(letters[1:3]), force = TRUE),
                               as.list(letters[1:3]))

  vec_cls <- as_json_vec(list(json_class(a = "b", class = "foo"),
                              json_class(a = "c", class = "foo"),
                              json_class(a = "d", class = "foo"),
                              json_class(a = "e", class = "foo")))
  expect_s3_class(vec_cls, "json_vec")
  expect_s3_class(vec_cls, "foo")
  expect_length(vec_cls, 4)

  vec_vec <- as_json_vec(json_vec(json_class(a = "b", class = "foo"),
                                  json_class(a = "c", class = "foo"),
                                  json_class(a = "d", class = "foo"),
                                  json_class(a = "e", class = "foo")))
  expect_identical(vec_cls, vec_vec)

  vec_lec <- as_json_vec(list(json_vec(json_class(a = "b", class = "foo"),
                                       json_class(a = "c", class = "foo"),
                                       json_class(a = "d", class = "foo"),
                                       json_class(a = "e", class = "foo"))))
  expect_identical(vec_cls, vec_lec)

  vec_vcc <- as_json_vec(list(json_vec(json_class(a = "b", class = "foo"),
                                       json_class(a = "c", class = "foo")),
                              json_vec(json_class(a = "d", class = "foo"),
                                       json_class(a = "e", class = "foo"))))
  expect_identical(vec_cls, vec_vcc)

  vec_vls <- as_json_vec(list(json_class(a = "b", class = "foo"),
                              json_vec(json_class(a = "c", class = "foo"),
                                       json_class(a = "d", class = "foo")),
                              json_class(a = "e", class = "foo")))
  expect_identical(vec_cls, vec_vls)
})

test_that("json_vec helpers work", {
  expect_true(has_common_subclass(
    structure(list("a"), class = c("foo", "json_class"))))
  expect_true(has_common_subclass(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "json_class")))))
  expect_false(has_common_subclass(list(
    structure(list("a"), class = c("foo")),
    structure(list("b"), class = c("foo")))))
  expect_false(has_common_subclass(
    structure(list("a"), class = c("foo"))))
  expect_false(has_common_subclass(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("bar", "json_class")))))
  expect_false(has_common_subclass(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class")))))

  expect_equal(get_subclass(
    structure(list("a"), class = c("foo", "json_class"))), "foo")
  expect_equal(get_subclass(
    structure(list("a"), class = c("foo", "bar", "json_class"))),
    c("foo", "bar"))
  expect_error(get_subclass("a"))
  expect_error(get_subclass(structure(list("a"), class = c("foo"))))
  expect_error(get_subclass(structure(list("a"),
               class = c("foo", "bar"))))
  expect_equal(get_subclass(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "json_class")))), "foo")
  expect_equal(get_subclass(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class")))),
    c("foo", "bar"))
  expect_error(get_subclass(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("bar", "foo", "json_class")))))
  expect_error(get_subclass(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("bar", "json_class")))))
  expect_error(get_subclass(list(
    structure(list("a"), class = c("json_class")),
    structure(list("b"), class = c("json_class")))))
  expect_error(get_subclass(list(
    structure(list("a"), class = c("foo")),
    structure(list("b"), class = c("foo")))))
  expect_equal(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "json_class")))), c("foo"))
  expect_equal(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "json_class")))), c("foo"))
  expect_equal(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class")))),
    c("foo", "bar"))
  expect_error(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "json_class")))))
  expect_error(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("bar", "foo", "json_class")))))
  expect_error(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "bar")),
    structure(list("b"), class = c("foo", "bar", "json_class")))))
  expect_error(get_subclass(json_vec(
    structure(list("a"), class = c("foo")),
    structure(list("b"), class = c("foo")))))
  expect_error(get_subclass(json_vec(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("bar", "json_class")))))

  expect_error(get_subclass(c("a", "b")))

  expect_true(is.json_vec(json_vec(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "json_class")))))
  expect_true(is.json_vec(json_vec(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class")))))
  expect_true(is.json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class"))),
    class = c("foo", "bar", "json_vec"))))
  expect_false(is.json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class"))),
    class = c("foo", "bar"))))
  expect_false(is.json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class"))),
    class = c("bar", "foo", "json_vec"))))
  expect_false(is.json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class"))),
    class = c("foo", "json_vec", "bar"))))
  expect_false(is_json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar")),
    structure(list("b"), class = c("foo", "bar", "json_class"))),
    class = c("foo", "bar", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("foo", "bar", "json_class"))),
    class = c("json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure(list("a"), class = c("foo", "bar", "json_class")),
    structure(list("b"), class = c("bar", "foo", "json_class"))),
    class = c("foo", "bar", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("bar", "json_class"))),
    class = c("foo", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "json_class"))),
    class = c("bar", "json_vec"))))
  expect_false(is_json_vec(structure(list(
    structure(list("a"), class = c("foo", "json_class")),
    structure(list("b"), class = c("foo", "json_class"))),
    class = c("json_vec"))))
})
