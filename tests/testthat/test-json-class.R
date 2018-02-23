context("json_class objects")

test_that("json objects can be converted", {
  expect_equal(NULL, as_json_class(list()))
  expect_equal(list("a", "b"), as_json_class(list("a", "b")))
  expect_error(as_json_class("a"))

  expect_equal(list(), rm_json_class(list()))
  expect_equal(list("a", "b"), rm_json_class(list("a", "b")))
  expect_equal("a", rm_json_class("a"))

  expect_s3_class(as_json_class(list(`@type` = "foo", "a", "b")), "json_class")
  expect_s3_class(as_json_class(list(`@type` = "foo", "a", "b")), "foo")

  lst <- list(`@type` = "foo",
              `@id` = 1L,
              bar = list(a = "b",
                         c = "d",
                         foo = list(`@type` = "xyz",
                                    `@id` = 5L,
                                    e = "f"),
                         g = "h"),
              foobar = list(`@type` = "bar",
                            `@id` = 3L,
                            j = "k"))

  expect_s3_class(as_json_class(lst), "foo")
  expect_s3_class(as_json_class(lst), "json_class")
  expect_s3_class(as_json_class(lst)$foobar, "bar")
  expect_s3_class(as_json_class(lst)$foobar, "json_class")
  expect_s3_class(as_json_class(lst)$bar$foo, "xyz")
  expect_s3_class(as_json_class(lst)$bar$foo, "json_class")
  expect_equal(sum(grepl("@type", names(unlist(lst)))), 3L)
  expect_equal(sum(grepl("@type", names(unlist(as_json_class(lst))))), 0L)

  expect_equal(lst, rm_json_class(lst))

  expect_s3_class(tmp <- as_json_class(lst), "json_class")
  expect_type(tmp <- rm_json_class(tmp), "list")
  expect_equal(tmp, rm_json_class(as_json_class(tmp)))

  expect_error(as_json_class(list(`@type` = "foo", `@type` = "bar", a = "b")))

  lst <- list(foo = list(`@type` = "foobar",
                         a = "b"),
              bar = list(`@type` = "foobar",
                         c = "d"))
  expect_s3_class(as_json_class(lst)[[1]], "foobar")
  expect_s3_class(as_json_class(lst)[[1]], "json_class")
  expect_s3_class(as_json_class(lst)[[2]], "foobar")
  expect_s3_class(as_json_class(lst)[[2]], "json_class")
  expect_true(all(sapply(as_json_class(lst), class)[1, ] == "foobar"))
  expect_equal(lst, rm_json_class(as_json_class(lst)))
  expect_error(as_json_class(list(`@type` = c("foo", "bar"), "a", "b")))
  expect_identical(as_json_class(lst), as_json_class(as_json_class(lst)))
  cls <- structure(list("a", "b"), class = c("foo", "json_class"))
  expect_identical(cls, as_json_class(cls))
  expect_identical(cls, as.json_class(as.json_vec(cls)))
  expect_identical(cls, as.list(cls))
  expect_identical(as_json_class(lst), lapply(as_json_class(lst), as_list))
  expect_identical(lst, lapply(as_json_class(lst), as_list, keep_asis = FALSE))
  lst <- list(`@type` = "foo", "a", "b")
  expect_identical(lst, as.list(as.json_class(lst), keep_asis = FALSE))
  a <- structure(list("a"), class = c("foo", "json_class"))
  b <- structure(list("b"), class = c("foo", "json_class"))
  expect_s3_class(c(a, b), "foo")
  expect_s3_class(c(a, b), "json_vec")
})

test_that("json objects can be created", {
  cls <- json_class("a", "b", class = "foo")
  expect_s3_class(cls, "foo")
  expect_s3_class(cls, "json_class")
})

test_that("json objects can be tested", {
  expect_false(has_subclass(list(`@type` = "foo", "a", "b"), "foo"))
  expect_true(has_subclass(as_json_class(list(`@type` = "foo", "a", "b")),
                                "foo"))
  expect_false(has_subclass(list(`@type` = "foo", "a", "b"), "foo"))

  expect_false(is_json_class(list(`@type` = "foo", "a", "b")))
  expect_true(is_json_class(as_json_class(list(`@type` = "foo", "a", "b"))))
  expect_true(is_json_class(structure(list("a", "b"),
                                      class = c("foo", "json_class"))))
  expect_false(is_json_class(structure(c("a", "b"),
                                       class = c("foo", "json_class"))))

  expect_true(check_json_class(structure(list("a", "b"),
                                         class = c("foo", "json_class"))))
  expect_false(check_json_class(structure(c("a", "b"),
                                          class = c("foo", "json_class"))))
  expect_true(check_json_class(
    structure(list("a",
                   structure(list("b"), class = c("bar", "json_class"))),
              class = c("foo", "json_class"))))
  expect_false(check_json_class(
    structure(list("a",
                   structure("b", class = c("bar", "json_class"))),
              class = c("foo", "json_class"))))

})

test_that("json subclass can be determined", {
  expect_error(get_subclass("a"))
  expect_error(get_subclass(list(`@type` = "foo", "a", "b")))
  expect_equal(
    get_subclass(as_json_class(list(`@type` = "foo", "a", "b"))), "foo")
  expect_equal(get_subclass(structure(list("a", "b"),
                                           class = c("foo", "json_class"))),
               "foo")
  expect_equal(
    get_subclass(structure(list("a", "b"),
                                class = c("foo", "bar", "json_class"))),
    c("foo", "bar"))
})

test_that("json objects can be subsetted", {
  lst <- list(foo = list(`@type` = "foobar",
                         a = "c",
                         b = "d"),
              bar = list(`@type` = "foobar",
                         a = "e",
                         b = "f"))
  expect_true(all(sapply(lapply(as_json_class(lst), `[`, "b"),
                         class)[1, ] == "foobar"))
  expect_true(all(sapply(lapply(as_json_class(lst), `[`, "b"),
                         class)[2, ] == "json_class"))
  lst <- lapply(as_json_class(lst), `class<-`, "some_class")
  expect_true(all(sapply(lst, class) == "some_class"))
  expect_false(all(sapply(lapply(lst, `[`, "b"), class) == "some_class"))
})

test_that("json objects can be rep'd", {
  cls <- json_class(a = "b", c = "d", class = "foo")

  cls_2 <- rep(cls, 2)

  expect_is(cls_2, "foo")
  expect_is(cls_2, "json_vec")
  expect_identical(cls_2[[1]], cls_2[[2]])
  expect_equal(length(cls_2), 2L)

  cls_4 <- rep(cls, 2, each = 2)

  expect_is(cls_4, "foo")
  expect_is(cls_4, "json_vec")
  expect_identical(cls_4[[1]], cls_4[[2]])
  expect_equal(length(cls_4), 4L)
})