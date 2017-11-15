context("openbis login")

cred <- load_config(section = "openbis")

test_that("openbis login is possible", {
  expect_error(login_openbis())
  expect_error(login_openbis("foo", "bar"))
  expect_error(login_openbis(cred$username, cred$password, api = "foo"))
  expect_error(login_openbis(cred$username, cred$password, url = "foo"))
  expect_error(login_openbis("foo", "bar", url = "google.ch"))
  expect_error(login_openbis("foo", "bar", foo = "google.ch"))
  expect_type(tok <- login_openbis(cred$username, cred$password,
                                   auto_disconnect = FALSE), "character")
  expect_true(grepl(paste0("^", cred$username), tok))
  expect_true(is_token_valid(tok))
  expect_null(logout_openbis(tok))
  expect_false(is_token_valid(tok))
  tok <- login_openbis(cred$username, cred$password,
                       host = "https://infectx.biozentrum.unibas.ch")
  tok_chr <- as.character(tok)
  expect_true(is_token_valid(tok_chr))
  rm(tok)
  gc()
  expect_false(is_token_valid(tok_chr))
})
