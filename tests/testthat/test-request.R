context("json requests")

test_that("default requests work", {
  req_1 <- do_requests_serial("https://httpbin.org/ip")
  expect_is(req_1, "list")
  expect_length(req_1, 1L)
  expect_is(req_1[[1]], "raw")
  expect_is(rawToChar(req_1[[1]]), "character")
  expect_match(rawToChar(req_1[[1]]), "origin")

  req_2 <- do_requests_serial(c("https://httpbin.org/ip",
                                "https://httpbin.org/uuid"))
  expect_is(req_2, "list")
  expect_length(req_2, 2L)
  for (i in seq_along(req_2)) {
    expect_is(req_2[[i]], "raw")
    expect_is(rawToChar(req_2[[i]]), "character")
  }
  expect_match(rawToChar(req_2[[1]]), "origin")
  expect_match(rawToChar(req_2[[2]]), "uuid")

  req_1 <- do_requests_parallel("https://httpbin.org/ip")
  expect_is(req_1, "list")
  expect_length(req_1, 1L)
  expect_is(req_1[[1]], "raw")
  expect_is(rawToChar(req_1[[1]]), "character")
  expect_match(rawToChar(req_1[[1]]), "origin")

  req_2 <- do_requests_parallel(c("https://httpbin.org/ip",
                                  "https://httpbin.org/uuid"))
  expect_is(req_2, "list")
  expect_length(req_2, 2L)
  for (i in seq_along(req_2)) {
    expect_is(req_2[[i]], "raw")
    expect_is(rawToChar(req_2[[i]]), "character")
  }
  expect_match(rawToChar(req_2[[1]]), "origin")
  expect_match(rawToChar(req_2[[2]]), "uuid")
})

test_that("requests can be customized", {
  urls <- rep("https://httpbin.org/post", 2)
  json <- list(list(a = "foo"),
               list(a = "bar"))

  post_handle <- function(x)
    curl::handle_setheaders(
      curl::new_handle(postfields = charToRaw(jsonlite::toJSON(x))),
      "Content-Type" = "application/json"
    )
    
  process_json <- function(x) 
    jsonlite::fromJSON(rawToChar(x))$json

  req_1 <- do_requests_serial(urls[1], list(json[[1]]),
                              create_handle = post_handle,
                              finally = process_json)
  expect_identical(req_1[[1]], json[[1]])

  req_2 <- do_requests_serial(urls, json,
                              create_handle = post_handle,
                              finally = process_json)
  expect_identical(req_2, json)

  req_1 <- do_requests_parallel(urls[1], list(json[[1]]),
                                create_handle = post_handle,
                                finally = process_json)
  expect_identical(req_1[[1]], json[[1]])

  req_2 <- do_requests_parallel(urls, json,
                                create_handle = post_handle,
                                finally = process_json)
  expect_identical(req_2, json)

  urls <- paste0("https://httpbin.org/status/", c(200, 400, 404))
  check_error <- function(x, ...) {
    if (x$status_code == 400)
      simpleError("retry")
    else if (x$status_code == 404)
      "foo"
    else
      "bar"
  }

  req_1 <- do_requests_serial(urls[1], check = check_error)
  expect_is(req_1, "list")
  expect_length(req_1, 1L)
  expect_is(req_1[[1]], "character")
  expect_equal(req_1[[1]], "bar")

  req_2 <- do_requests_serial(urls[c(1, 3)], check = check_error)
  expect_is(req_2, "list")
  expect_length(req_2, 2L)
  expect_equal(req_2, list("bar", "foo"))

  expect_warning(req_1 <- do_requests_serial(urls[2], check = check_error),
                 "could not carry out request")
  expect_is(req_1, "list")
  expect_length(req_1, 1L)
  expect_null(req_1[[1]])

  expect_warning(req_3 <- do_requests_serial(urls, check = check_error),
                 "could not carry out request")
  expect_is(req_3, "list")
  expect_length(req_3, 3L)
  expect_equal(req_3[[1]], "bar")
  expect_null(req_3[[2]])
  expect_equal(req_3[[3]], "foo")
})

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
  expect_identical(
    resolve_references(as_json_class(list(`@type` = "foo", `@id` = 1L))),
    as_json_class(list(`@type` = "foo")))
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
