context("json objects")

test_that("json objects can be converted", {
  expect_equal("a", as_json_class("a"))
  expect_equal(list(), as_json_class(list()))
  expect_equal(list("a", "b"), as_json_class(list("a", "b")))

  expect_equal("a", as_json_list("a"))
  expect_equal(list(), as_json_list(list()))
  expect_equal(list("a", "b"), as_json_list(list("a", "b")))

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

  expect_s3_class(as_json_class(lst), c("foo", "json_class"))
  expect_s3_class(as_json_class(lst)$foobar, c("bar", "json_class"))
  expect_s3_class(as_json_class(lst)$bar$foo, c("xyz", "json_class"))
  expect_equal(sum(grepl("@id", names(unlist(lst)))), 3L)
  expect_equal(sum(grepl("@id", names(unlist(as_json_class(lst))))), 0L)
  expect_equal(sum(grepl("@type", names(unlist(lst)))), 3L)
  expect_equal(sum(grepl("@type", names(unlist(as_json_class(lst))))), 0L)

  expect_equal(lst, as_json_list(lst))

  expect_s3_class(tmp <- as_json_class(lst), "json_class")
  expect_type(tmp <- as_json_list(tmp), "list")
  expect_equal(tmp, as_json_list(as_json_class(tmp)))

  expect_error(as_json_class(list(`@type` = "foo", `@type` = "bar", a = "b")))

  lst <- list(foo = list(`@type` = "foobar",
                         a = "b"),
              bar = list(`@type` = "foobar",
                         c = "d"))
  expect_s3_class(as_json_class(lst)[[1]], "json_class")
  expect_s3_class(as_json_class(lst)[[2]], "json_class")
  expect_true(all(sapply(as_json_class(lst), class)[1, ] == "foobar"))
  expect_equal(lst, as_json_list(as_json_class(lst)))
})

test_that("json objects can be tested", {
  expect_false(has_json_subclass(list(`@type` = "foo", "a", "b")))
  expect_true(has_json_subclass(as_json_class(list(`@type` = "foo", "a", "b")),
                                "foo"))
  expect_false(has_json_subclass(list(`@type` = "foo", "a", "b"), "foo"))
  expect_false(is_json_class(list(`@type` = "foo", "a", "b")))
  expect_true(is_json_class(as_json_class(list(`@type` = "foo", "a", "b"))))
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

test_that("json objects can be printed", {
  expect_is(proj <- list_projects(tok), "list")
  expect_output(print(proj[[1]], depth = Inf))
  expect_output(print(proj[[1]], depth = Inf), "EntityRegistrationDetails")
  expect_output(print(proj[[1]], depth = Inf), "█─")
  expect_output(print(proj[[1]], fancy = FALSE), "X-")
  expect_output(print(proj[[1]]), "\\.\\.\\.")
  expect_output(print(proj[[1]], depth = Inf, length = 10), "\\.\\.\\.")
  expect_output(print(proj[[1]], depth = Inf, width = 50), "\\.\\.\\.")
})