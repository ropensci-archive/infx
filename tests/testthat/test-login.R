context("openbis login")

test_that("openbis login is possible", {
  expect_error(login_openbis("foo", "bar"), "Login failed")

  token <- login_openbis()
  expect_is(token, "character")
  expect_is(attr(token, "finaliser"), "environment")
  expect_is(attr(token, "host_url"), "character")
  expect_match(token, "^rdgr2014")
  expect_true(is_token_valid(token))
  expect_null(logout_openbis(token))
  expect_false(is_token_valid(token))

  token <- login_openbis("rdgr2014", "IXPubReview")
  tok_chr <- as.character(token)
  attr(tok_chr, "host_url") <- attr(token, "host_url")
  expect_true(is_token_valid(token))
  expect_true(is_token_valid(tok_chr))
  rm(token)
  gc()
  expect_false(is_token_valid(tok_chr))

  token <- login_openbis("test_observer", "test_observer",
                         auto_disconnect = FALSE,
                         host_url = "https://openbis.elnlims.ch")

  expect_is(token, "character")
  expect_length(token, 1L)
  expect_match(token, "^test_observer-")

  expect_true(is_token_valid(token))
  expect_null(logout_openbis(token))
  expect_false(is_token_valid(token))
})
