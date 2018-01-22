context("json requests")

test_that("id fields are stripped", {
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
  jsn <- resolve_references(as_json_class(lst))


  expect_true(any(grepl("@id", names(unlist(lst)))))
  expect_false(any(grepl("@id", names(unlist(jsn)))))
  expect_identical(jsn, resolve_references(jsn))
  expect_identical(
    resolve_references(as_json_class(list(
      `@type` = "foo", `@id` = 1L, a = "a"))),
    json_class(a = "a", class = "foo"))
})

test_that("references are resolved", {
  lst <- list(list(`@type` = "foo",
                   `@id` = 1L,
                   a = "b",
                   c = "d",
                   e = list(`@type` = "bar",
                            `@id` = 2L,
                            f = "g")),
              list(`@type` = "foo",
                   `@id` = 3L,
                   a = "h",
                   c = "i",
                   e = 2L))
  jsn <- resolve_references(as_json_class(lst))

  expect_identical(lst[[2]][["e"]], 2L)
  expect_identical(jsn[[2]][["e"]], jsn[[1]][["e"]])
})
