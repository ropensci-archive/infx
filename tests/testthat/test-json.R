context("json objects")

test_that("json objects can be converted", {
  expect_equal("a", json_class("a"))
  expect_equal(list(), json_class(list()))
  expect_equal(list("a", "b"), json_class(list("a", "b")))

  expect_equal("a", json_class("a", mode = "rm"))
  expect_equal(list(), json_class(list(), mode = "rm"))
  expect_equal(list("a", "b"), json_class(list("a", "b"), mode = "rm"))

  expect_s3_class(json_class(list(`@type` = "foo", "a", "b")), "json_class")
  expect_equal(attr(json_class(list(`@type` = "foo", "a", "b")), "json_class"),
               "foo")

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
  expect_s3_class(json_class(lst), "json_class")
  expect_s3_class(json_class(lst)$foobar, "json_class")
  expect_s3_class(json_class(lst)$bar$foo, "json_class")
  expect_equal(sum(grepl("@id", names(unlist(lst)))), 3L)
  expect_equal(sum(grepl("@id", names(unlist(json_class(lst))))), 0L)
  expect_equal(sum(grepl("@type", names(unlist(lst)))), 3L)
  expect_equal(sum(grepl("@type", names(unlist(json_class(lst))))), 0L)

  expect_equal(lst, rm_json_class(lst))

  expect_s3_class(tmp <- add_json_class(lst), "json_class")
  expect_type(tmp <- rm_json_class(tmp), "list")
  expect_equal(tmp, rm_json_class(add_json_class(tmp)))

  expect_error(json_class(list(`@type` = "foo", `@type` = "bar", a = "b")))

  lst <- list(foo = list(`@type` = "foobar",
                         a = "b"),
              bar = list(`@type` = "foobar",
                         c = "d"))
  expect_s3_class(json_class(lst)[[1]], "json_class")
  expect_s3_class(json_class(lst)[[2]], "json_class")
  expect_true(all(sapply(json_class(lst), attr, "json_class") == "foobar"))
  expect_equal(lst, rm_json_class(add_json_class(lst)))
})

test_that("json objects can be tested", {
  expect_false(has_json_class(list(`@type` = "foo", "a", "b")))
  expect_true(has_json_class(json_class(list(`@type` = "foo", "a", "b"))))
  expect_false(has_json_class(list(`@type` = "foo", "a", "b"), "foo"))
  expect_true(has_json_class(json_class(list(`@type` = "foo", "a", "b")),
                             "foo"))
  expect_false(is_json_class(list(`@type` = "foo", "a", "b")))
  expect_true(is_json_class(json_class(list(`@type` = "foo", "a", "b"))))
})

test_that("json objects can be subsetted", {
  lst <- list(foo = list(`@type` = "foobar",
                         a = "c",
                         b = "d"),
              bar = list(`@type` = "foobar",
                         a = "e",
                         b = "f"))
  expect_true(all(sapply(lapply(json_class(lst), `[`, "b"), attr,
                         "json_class") == "foobar"))
  lst <- lapply(json_class(lst), `class<-`, NULL)
  expect_true(all(sapply(lst, attr, "json_class") == "foobar"))
  expect_true(all(sapply(lapply(lst, `[`, "b"), attr,
                         "json_class") == NULL))
})