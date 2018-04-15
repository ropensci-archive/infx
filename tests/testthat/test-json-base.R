context("base R generics for json objects")

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

  expect_s3_class(cls_2, "foo")
  expect_s3_class(cls_2, "json_vec")
  expect_identical(cls_2[[1]], cls_2[[2]])
  expect_length(cls_2, 2L)

  cls_4 <- rep(cls, 2, each = 2)

  expect_s3_class(cls_4, "foo")
  expect_s3_class(cls_4, "json_vec")
  expect_identical(cls_4[[1]], cls_4[[2]])
  expect_length(cls_4, 4L)
})

test_that("base generics work for json vecs", {
  cls_b <- json_class(a = "b", c = "d", class = "foo")
  cls_e <- json_class(a = "e", c = "f", class = "foo")

  cls_be <- c(cls_b, cls_e)
  expect_s3_class(cls_be, "foo")
  expect_s3_class(cls_be, "json_vec")
  expect_length(cls_be, 2L)

  cls_be[[2]] <- cls_b
  expect_s3_class(cls_be, "foo")
  expect_s3_class(cls_be, "json_vec")
  expect_length(cls_be, 2L)
  expect_identical(cls_be[[1]], cls_be[[2]])

  cls_2 <- rep(c(cls_b, cls_e), 2)

  expect_s3_class(cls_2, "foo")
  expect_s3_class(cls_2, "json_vec")
  expect_identical(cls_2[[1]], cls_2[[3]])
  expect_identical(cls_2[[2]], cls_2[[4]])
  expect_length(cls_2, 4L)

  cls_4 <- rep(c(cls_b, cls_e), 2, each = 2)

  expect_s3_class(cls_4, "foo")
  expect_s3_class(cls_4, "json_vec")
  expect_identical(cls_4[[1]], cls_4[[2]])
  expect_identical(cls_4[[1]], cls_4[[5]])
  expect_identical(cls_4[1:2], cls_4[5:6])
  expect_length(cls_4, 8L)
})

test_that("json objects can be printed", {
  expect_output(print(structure(list("a"), class = c("foo", "json_class")),
                      length = 1L), "...")
  expect_output(print(structure(list(a = "a", b = list(c = "d", e = "f")),
                                class = c("foo", "json_class"))),
                "[c = d, e = f]")
  expect_output(print(structure(list(a = "a", b = list(c = "d", "f")),
                                class = c("foo", "json_class"))),
                "[c = d, f]")
  expect_output(print(structure(list(a = "a", b = c("d", "f")),
                                class = c("foo", "json_class"))),
                "(d, f)")
  expect_output(print(
    structure(list(a = "a",
                   list(c = "c",
                        structure(list("b"), class = c("bar", "json_class")))),
              class = c("foo", "json_class")), depth = 3, fancy = FALSE),
    "\\-+-c = c \n  \\-X-bar", fixed = TRUE)

  skip_on_appveyor()

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
  expect_output(print(json_class(c = "d", e = list(json_class(a = "b",
                                                              class = "foo")),
                                 class = "bar"), 2L),
                "└─e = ──█─foo")
  expect_output(print(json_class(c = "d", list(json_class(a = "b",
                                                          class = "foo")),
                                 class = "bar"), 2L),
                "└──█─foo")
})

test_that("json_vec printing works", {

  skip_on_appveyor()

  expect_output(print(
    structure(list(structure(list("a"), class = c("foo", "json_class"))),
              class = c("json_vec"))),
    "──█─foo")
  expect_output(print(
    structure(list(structure(list("a"), class = c("foo", "json_class")),
                   structure(list("b"), class = c("foo", "json_class"))),
              class = c("json_vec"))),
    "┌─█─foo \n│ └─a \n└─█─foo")
  expect_output(print(
    structure(list(structure(list("a"), class = c("foo", "json_class")),
                   structure(list("b"), class = c("foo", "json_class")),
                   structure(list("c"), class = c("foo", "json_class"))),
              class = c("json_vec"))),
    "┌─█─foo \n│ └─a \n├─█─foo \n│ └─b \n└─█─foo \n  └─c")
  expect_output(print(
    structure(list(structure(list("a"), class = c("foo", "json_class")),
                   structure(list("b"), class = c("foo", "json_class"))),
              class = c("json_vec")), length = 2),
    "┌─█─foo \n...")
  expect_output(print(
    structure(list(structure(list(paste(letters, collapse = "")),
                             class = c("foo", "json_class")),
                   structure(list("b"), class = c("foo", "json_class"))),
              class = c("json_vec"))),
    "│ └─abcdefghijklmnopqrstuvwxyz")
  expect_output(print(
    structure(list(structure(list(paste(letters, collapse = "")),
                             class = c("foo", "json_class")),
                   structure(list("b"), class = c("foo", "json_class"))),
              class = c("json_vec")), width = 20),
    "│ └─abcdefghijklm...")
})
