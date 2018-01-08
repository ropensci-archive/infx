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

  expect_true(any(grepl("@id", names(unlist(lst)))))
  expect_false(any(grepl("@id", names(unlist(remove_id(lst))))))
  expect_identical(remove_id(lst), remove_id(remove_id(lst)))
  expect_identical(remove_id("a"), "a")
  expect_identical(remove_id(list(a = "a", `@id` = 1L)), list(a = "a"))
  expect_identical(remove_id(list(`@id` = 1L)), setNames(list(), character()))
})
