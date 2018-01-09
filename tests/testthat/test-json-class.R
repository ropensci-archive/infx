context("json objects")

test_that("json objects can be converted", {
  expect_equal("a", json_class("a"))
  expect_equal(list(), json_class(list()))
  expect_equal(list("a", "b"), json_class(list("a", "b")))

  expect_equal("a", rm_json_class("a"))
  expect_equal(list(), rm_json_class(list()))
  expect_equal(list("a", "b"), rm_json_class(list("a", "b")))

  expect_s3_class(json_class(list(`@type` = "foo", "a", "b")), "json_class")
  expect_s3_class(json_class(list(`@type` = "foo", "a", "b")), "foo")

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

  expect_s3_class(json_class(lst), c("foo", "json_class"))
  expect_s3_class(json_class(lst)$foobar, c("bar", "json_class"))
  expect_s3_class(json_class(lst)$bar$foo, c("xyz", "json_class"))
  expect_equal(sum(grepl("@type", names(unlist(lst)))), 3L)
  expect_equal(sum(grepl("@type", names(unlist(json_class(lst))))), 0L)

  expect_equal(lst, rm_json_class(lst))

  expect_s3_class(tmp <- json_class(lst), "json_class")
  expect_type(tmp <- rm_json_class(tmp), "list")
  expect_equal(tmp, rm_json_class(json_class(tmp)))

  expect_error(json_class(list(`@type` = "foo", `@type` = "bar", a = "b")))

  lst <- list(foo = list(`@type` = "foobar",
                         a = "b"),
              bar = list(`@type` = "foobar",
                         c = "d"))
  expect_s3_class(json_class(lst)[[1]], "json_class")
  expect_s3_class(json_class(lst)[[2]], "json_class")
  expect_true(all(sapply(json_class(lst), class)[1, ] == "foobar"))
  expect_equal(lst, rm_json_class(json_class(lst)))
  expect_error(json_class(list(`@type` = c("foo", "bar"), "a", "b")))
})

test_that("json objects can be tested", {
  expect_false(has_json_subclass(list(`@type` = "foo", "a", "b")))
  expect_true(has_json_subclass(json_class(list(`@type` = "foo", "a", "b")),
                                "foo"))
  expect_false(has_json_subclass(list(`@type` = "foo", "a", "b"), "foo"))

  expect_false(is_json_class(list(`@type` = "foo", "a", "b")))
  expect_true(is_json_class(json_class(list(`@type` = "foo", "a", "b"))))
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
  expect_error(get_json_subclass("a"))
  expect_error(get_json_subclass(list(`@type` = "foo", "a", "b")))
  expect_equal(
    get_json_subclass(json_class(list(`@type` = "foo", "a", "b"))), "foo")
  expect_equal(get_json_subclass(structure(list("a", "b"),
                                           class = c("foo", "json_class"))),
               "foo")
  expect_equal(
    get_json_subclass(structure(list("a", "b"),
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
  expect_true(all(sapply(lapply(json_class(lst), `[`, "b"),
                         class)[1, ] == "foobar"))
  expect_true(all(sapply(lapply(json_class(lst), `[`, "b"),
                         class)[2, ] == "json_class"))
  lst <- lapply(json_class(lst), `class<-`, "some_class")
  expect_true(all(sapply(lst, class) == "some_class"))
  expect_false(all(sapply(lapply(lst, `[`, "b"), class) == "some_class"))
})

test_that("json objects can be printed", {
  expect_output(print(structure(list("a"), class = c("foo", "json_class"))),
                "█─foo \n└─a")
  expect_output(print(structure(list(a = "a", "b"),
                                class = c("foo", "json_class"))),
                "├─a = a \n└─b")
  expect_output(print(
    structure(list(a = "a",
                   list(structure(list("b"), class = c("bar", "json_class")),
                        "c")),
              class = c("foo", "json_class"))), "└─...")
  expect_output(print(
    structure(list(a = "a",
                   list(structure(list("b"), class = c("bar", "json_class")),
                        "c")),
              class = c("foo", "json_class")), depth = 3),
    "└─┬─█─bar \n  │ └─b")
  expect_output(print(
    structure(list(a = "a",
                   list(c = "c",
                        structure(list("b"), class = c("bar", "json_class")))),
              class = c("foo", "json_class")), depth = 3),
    "└─┬─c = c \n  └─█─bar")

  expect_warning(print(structure("a", class = c("foo", "json_class"))))
})