context("utils")

test_that("config file can be loaded", {
  expect_is(load_config(), "list")
  expect_named(load_config())
  expect_gte(length(load_config()), 1)

  test <- list(foo = "foo",
               foobar = c("foo", "bar"))

  writeLines(yaml::as.yaml(test), con = "foo.yaml")
  expect_equal(load_config(), test)
  unlink("foo.yaml")

  writeLines(yaml::as.yaml(test), con = "foo.yml")
  expect_equal(load_config(), test)
  unlink("foo.yml")

  tmp <- load_config()
  writeLines(yaml::as.yaml(test), con = "foo.bar")
  expect_equal(load_config(), tmp)
  unlink("foo.bar")

  sec <- list(a = 1,
              b = list(c = 2, d = 3),
              e = list(f = list(g = 4, h = 5),
                       i = 6),
              j = 7,
              k = list(l = 8,
                       m = list(n = 9,
                                o = list(o = 10, p = 11),
                                q = 12),
                       r = 13),
                       m = list(1, 2),
              s = 14,
              c = 15,
              t = list(e = 16))
  writeLines(yaml::as.yaml(sec), con = "foo.yaml")
  expect_equal(load_config(section = "f"), list(g = 4, h = 5))
  expect_equal(load_config(section = "m"), 1:2)
  expect_equal(load_config(section = "c"), 15)
  expect_equal(load_config(section = "o"), list(o = 10, p = 11))
  expect_equal(load_config(section = "q"), 12)
  expect_equal(load_config(section = "d"), 3)
  expect_equal(load_config(section = "e"), list(f = list(g = 4, h = 5),
                                                       i = 6))
  expect_error(load_config(section = "z"))
  unlink("foo.yaml")
})
