context("json utils")

test_that("json objects can be checked for fields", {
  obj_1 <- json_class(a = 1, b = 2, class = "foo")
  obj_2 <- json_class(a = 3, b = 4, class = "foo")
  obj_3 <- json_class(a = 3, c = 4, class = "foo")

  expect_true(has_fields(obj_1, "a"))
  expect_true(has_fields(obj_1, c("a", "b")))
  expect_false(has_fields(obj_1, "c"))
  expect_false(has_fields(obj_1, c("a", "c")))

  expect_true(has_fields(c(obj_1, obj_2), "a"))
  expect_true(has_fields(c(obj_1, obj_2), c("a", "b")))
  expect_false(has_fields(c(obj_1, obj_2), "c"))
  expect_false(has_fields(c(obj_1, obj_2), c("a", "c")))

  expect_true(has_fields(c(obj_1, obj_3), "a"))
  expect_false(has_fields(c(obj_1, obj_3), c("a", "b")))
  expect_false(has_fields(c(obj_1, obj_3), "c"))
  expect_false(has_fields(c(obj_1, obj_3), c("a", "c")))

  expect_error(has_fields(obj_1, 1L))
  expect_error(has_fields(obj_1, character()))
  expect_false(has_fields(list(a = 1), "a"))
})

test_that("NULL fields can be filtered", {
  tmp <- json_class(a = json_class(b = "c", d = NULL, class = "foo"),
                    e = json_class(f = "g", class = "bar"),
                    h = NULL,
                    class = "foobar")
  tmp <- json_vec(tmp, tmp)

  expect_false(any(sapply(tmp, is.null)))
  expect_true(any(sapply(tmp[[1]], is.null)))
  expect_true(any(sapply(tmp[[1]]$a, is.null)))
  expect_false(any(sapply(tmp[[1]]$e, is.null)))

  tmp <- remove_null(tmp)
  expect_false(any(sapply(tmp, is.null)))
  expect_false(any(sapply(tmp[[1]], is.null)))
  expect_false(any(sapply(tmp[[1]]$a, is.null)))
  expect_false(any(sapply(tmp[[1]]$e, is.null)))

  expect_true(is_json_vec(tmp))
  expect_identical(get_subclass(tmp), "foobar")
  expect_true(all(sapply(tmp, is_json_class)))
})
