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
