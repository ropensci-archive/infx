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
  expect_error(has_fields(list(a = 1), "a"))
})

test_that("json objects can be printed", {
  expect_output(print(structure(list("a"), class = c("foo", "json_class"))),
                "█─foo \n└─a")
  expect_output(print(structure(list("a"), class = c("foo", "json_class")),
                      length = 1L), "...")
  expect_output(print(structure(list(a = "a", "b"),
                                class = c("foo", "json_class"))),
                "├─a = a \n└─b")
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
  expect_output(print(
    structure(list(a = "a",
                   list(c = "c",
                        structure(list("b"), class = c("bar", "json_class")))),
              class = c("foo", "json_class")), depth = 3, fancy = FALSE),
    "\\-+-c = c \n  \\-X-bar", fixed = TRUE)
})

test_that("json_vec printing work", {
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
