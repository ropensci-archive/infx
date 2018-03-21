
#' Make a JSON-RPC request
#' 
#' The functions powering all HTTP requests to OpenBIS are
#' `do_requests_serial()` for sequential calls and `do_requests_parallel()`
#' for asynchronous calls. Both take one or several urls, either as character
#' vector or list of unevaluated function calls which will be evaluated using
#' [base::eval()] shortly before being used (this is used for urls that are
#' only valid for a limited amount of time). The behavior of `do_requests_*()`
#' can be customized with the three functions passed as arguments
#' `create_handle`, `check` and `finally` together with the vector (of the
#' same length as `urls`) passed as argument `bodies`.
#' 
#' The function passed as `create_handle` receives one entry at the time of
#' the `bodies` object and is expected to return a curl handle created by
#' [curl::new_handle()]. The `check` function receives as first argument
#' the response of a single curl request alongside the corresponding entry of
#' the `bodies` object. This function should check whether the request was
#' successful or not, e.g. check the HTTP status code, size of the downloaded
#' file, etc. In case of failure it should return a `simpleError` error object,
#' created by [base::simpleError()] with message `retry` and in case of success
#' it should return the response data, e.g. the `content` entry of a curl
#' response. The third function, `finally`, is applied to the object returned
#' by the `check` function (in case of success) and can be used to parse JSON,
#' read a binary file, etc.
#' 
#' Both `do_requests_serial()` and `do_requests_parallel()` have the option of
#' retrying failed requests and the number of allowed retries can be controlled
#' with the argument `n_try`. Furthermore, `do_requests_parallel()` offers
#' control over the number of simultaneous connections using the argument
#' `n_con` and it has the option of performing the requests in a chunked
#' manner. This means that instead of adding all requests at once and letting
#' curl handle the queuing, only `n_con` requests are initially made and for
#' each successful one an additional request is added. This comes in handy for
#' urls that have a limited lifetime.
#'
#' The function `make_requests()` is used to construct JSON-RPC requests. The
#' arguments `methods`, `params`, `ids` and `version` are combined into one
#' or several request objects according to the JSON-RPC specification and
#' together with the `urls` argument are passed to `do_requests_*()`. The
#' objects passed as `urls`, `methods` and `params` should all be of the same
#' length but in case any are of length 1, they will be [base::rep()]'ed to
#' the required length. Care has to be taken that the list passed as `params`
#' has the correct degree of nesting. As `make_requests()` iterates over the
#' topmost list level, a single request should be wrapped in a list such that
#' the topmost list level is on length 1. The function `make_request()` is a
#' wrapper around `make_requests()` that does exactly this.
#' 
#' As part of the `process_json()` function, `@type` fields are converted
#' to`json_class` attributes, using [as_json_class()]. Additionally, `@id`
#' fields, which may be referenced if an objects is used multiple times, are
#' recursively resolved using [rm_json_class()] such that each object is
#' self-contained.
#' 
#' @param url,urls, Destination url(s), the request is sent to.
#' @param method,methods The API method name(s).
#' @param params A list structure holding the arguments which, converted to
#' JSON, will be used to call the supplied method. The `@type` entries will be
#' generated from `json_class` attributes.
#' @param ids Identifier(s) for the JSON-RPC request (defaults to a random
#' string of length 7). Can be usually be ignored, as only single JSON-RPC
#' requests are issued per HTTP request.
#' @param version JSON-RPC protocol version to be used (defaults to `"2.0"`.
#' @param n_con The number of simultaneous connections.
#' @param finally A function that is applied to the `result` entry of a
#' successful JSON RPC request.
#' 
#' @rdname request
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#'   projects <- make_request(api_url("gis"), "listProjects", list(token))
#'   print(projects[[1]])
#' }
#' 
#' @export
#' 
make_requests <- function(urls,
                          methods,
                          params,
                          ids = NULL,
                          version = "2.0",
                          n_con = 5L,
                          finally = process_json,
                          ...) {

  check_rep <- function(vec, len) {
    if (length(vec) == 1L)
      vec <- rep(vec, len)
    assert_that(length(vec) == len || length(vec) == 1L)
    vec
  }

  assert_that(is.list(params),
              all(sapply(params, is.list)))

  max_len <- max(length(urls), length(methods), length(params))

  if (max_len > 1L) {
    urls <- check_rep(urls, max_len)
    methods <- check_rep(methods, max_len)
    params <- check_rep(params, max_len)
  }

  if (is.null(ids))
    ids <- replicate(max_len, paste(sample(c(letters, LETTERS, 0:9), 7),
                                    collapse = ""))
  assert_that(length(ids) == max_len)

  bodies <- mapply(list,
                   id = ids,
                   jsonrpc = rep(version, max_len),
                   method = methods,
                   params = rm_json_class(params),
                   SIMPLIFY = FALSE)

  if (length(urls) > 1L && n_con > 1L)
    do_requests_parallel(urls, bodies, n_con,
                         create_handle = create_request_handle,
                         check = check_request_result, finally = finally, ...)
  else
    do_requests_serial(urls, bodies, create_handle = create_request_handle,
                        check = check_request_result, finally = finally, ...)
}

#' @param ... Further arguments to `make_request()` are passed to
#' `make_requests()` and from `make_requests()` to `do_requests_*()`.
#' 
#' @rdname request
#' @export
#' 
make_request <- function(url,
                         method,
                         params,
                         ...) {

  assert_that(length(url) == 1L,
              length(method) == 1L)

  make_requests(url, method, list(params), ...)[[1L]]
}


#' @param bodies Request bodies: a list where each entry is a list with slots
#' `id`, `jsonrpc`, `method` and `params`.
#' @param n_try Number of tries each request is performed in case of failed
#' requests.
#' @param create_handle A function that will receive a single entry of the
#' `bodies` list at the time and should return a curl handle created by
#' [curl::new_handle()].
#' @param check A function that receives both the result of a request and the
#' corresponding entry of the `bodies` list. Is expected to return NULL in
#' which case the request is retried or a list with an entry named `result`.
#' 
#' @rdname request
#' @export
#' 
do_requests_serial <- function(urls,
                               bodies = vector("list", length(urls)),
                               n_try = 2L,
                               create_handle = create_default_handle,
                               check = check_default_result,
                               finally = identity) {

  add_request <- function(i, tries) {

    if (tries <= 0L) {
      warning("could not carry out request within ", n_try, " tries.")
      return(invisible(NULL))
    }

    res <- curl::curl_fetch_memory(
      eval(urls[[i]]),
      handle = create_handle(bodies[[i]])
    )

    res <- check(res, bodies[[i]])

    if (inherits(res, "simpleError") && conditionMessage(res) == "retry")
      add_request(i, tries - 1L)
    else {
      res <- finally(res)
      if (length(urls) > 1L)
        pb$tick(1L)
      res
    }
  }

  assert_that(is.character(urls) || all(sapply(urls, is.call)),
              is.vector(bodies),
              length(urls) == length(bodies),
              is.count(n_try),
              is.function(create_handle),
              is.function(check),
              is.function(finally))

  if (length(urls) > 1L) {
    pb <- progress::progress_bar$new(
      format = paste0("querying [:bar] :percent in :elapsed"),
      total = length(urls))
    pb$tick(0)
  }

  lapply(seq_along(urls), add_request, n_try)
}

#' @param chunked Flag indicating whether to add downloads in chunks or all at
#' once. Can be used for download links which expire, such that they are only
#' created shortly before they are consumed.
#' 
#' @rdname request
#' @export
#' 
do_requests_parallel <- function(urls,
                                 bodies = vector("list", length(urls)),
                                 n_con = 5L,
                                 n_try = 2L,
                                 chunked = FALSE,
                                 create_handle = create_default_handle,
                                 check = check_default_result,
                                 finally = identity) {

  add_request <- function(i, tries) {

    if (chunked && (i < 1L || i > length(urls)))
      return(invisible(NULL))

    if (tries <= 0L) {
      warning("could not carry out request within ", n_try, " tries.")
      return(invisible(NULL))
    }

    curl::curl_fetch_multi(
      url = eval(urls[[i]]),
      handle = create_handle(bodies[[i]]),
      pool = pool,
      done = function(x) {

        resp <- check(x, bodies[[i]])

        if (inherits(resp, "simpleError") && conditionMessage(resp) == "retry")
          add_request(i, tries - 1L)
        else {
          if (chunked)
            add_request(i + n_con, n_try)
          res[[i]] <<- finally(resp)
          if (length(urls) > 1L)
            pb$tick(1L)
          invisible(NULL)
        }
      },
      fail = function(x) add_request(i, tries - 1L)
    )
  }

  assert_that(is.character(urls) || all(sapply(urls, is.call)),
              is.vector(bodies),
              length(urls) == length(bodies),
              is.count(n_con),
              is.count(n_try),
              is.flag(chunked),
              is.function(create_handle),
              is.function(check),
              is.function(finally))

  n_con <- min(n_con, length(urls))

  if (length(urls) > 1L) {
    pb <- progress::progress_bar$new(
      format = paste0("querying [:bar] :percent in :elapsed"),
      total = length(urls))
    pb$tick(0)
  }

  res <- vector("list", length(urls))

  pool <- curl::new_pool(host_con = n_con)

  start_inds <- if (chunked)
    seq.int(n_con)
  else
    seq_along(urls)

  sapply(start_inds, add_request, n_try)

  curl::multi_run(pool = pool)

  res
}

create_default_handle <- function(...)
  curl::new_handle()

check_default_result <- function(resp, ...) {

  if (resp$status_code != 200) {

    warning("request returned with code ", resp$status_code)
    simpleError("retry")

  } else {

    resp$content
  }
}

create_request_handle <- function(body) {

  body_raw <- charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))

  handle <- curl::new_handle(post = TRUE,
                             postfieldsize = length(body_raw),
                             postfields = body_raw)

  curl::handle_setheaders(handle, "Content-Type" = "application/json")
}

check_request_result <- function(resp, body) {

  if (resp$status_code != 200) {

    warning("request returned with code ", resp$status_code)
    simpleError("retry")

  } else {

    resp <- jsonlite::fromJSON(rawToChar(resp$content),
                               simplifyVector = FALSE)
    assert_that(resp$id == body$id)

    if (!is.null(resp$error)) {

      data <- resp$error$data[!grepl("^@", names(resp$error$data))]
      warning("\nerror with code ", resp$error$code, ":\n",
              paste(strwrap(paste(names(data), data, sep = ": "),
                            indent = 2L, exdent = 4L), collapse = "\n"))
      simpleError("retry")

    } else
      resp
  }
}

#' @param x A (possibly nested) list structure for which all `@type` fields
#' are turned into class attributes and `@id` fields are recursively removed.
#' 
#' @rdname request
#' @export
#' 
process_json <- function(x) {
  x <- x$result
  if (is.null(x))
    warning("an api call returned NULL.")
  x <- as_json_class(x, force = TRUE)
  x <- resolve_references(x)
  as_json_vec(x, force = TRUE)
}

#' @rdname request
#' @export
#' 
resolve_references <- function(x) {
  lookup <- unlist_objects(x)
  traverse_list(x, get_object_spec(lookup), lookup)
}

unlist_objects <- function(x) {

  gather_objs <- function(obj) {
    if (is.list(obj)) {
      if (is_json_class(obj))
        objects <<- c(objects, list(obj))
      sapply(obj, gather_objs)
    }
    invisible(NULL)
  }

  objects <- list()

  gather_objs(x)

  objects
}

get_object_spec <- function(x) {

  assert_that(is.list(x),
              all(sapply(x, is_json_class)))

  all_classes <- unique(sapply(x, get_subclass))
  obj_spec <- stats::setNames(vector(mode = "list",
                                     length = length(all_classes)),
                              all_classes)

  lapply(x, function(obj) {
    old <- obj_spec[[get_subclass(obj)]]
    new <- lapply(obj, class)
    all <- union(names(old), names(new))
    obj_spec[[get_subclass(obj)]] <<- stats::setNames(
      lapply(all, function(nme) unique(c(old[[nme]], new[[nme]]))), all)
    invisible(NULL)
  })

  obj_spec
}

assign_reference <- function(obj, spec, objects) {
  res <- mapply(function(x, y) {
    if (isTRUE(class(x) == "integer") && "json_class" %in% y) {
      z <- objects[[x]]
      assert_that(x == z[["@id"]],
                  isTRUE(get_subclass(z) %in% y))
      z
    } else
      x
  }, obj, spec[names(obj)], SIMPLIFY = FALSE)

  new_json_class(res, class = get_subclass(obj))
}

traverse_list <- function(x, specs, lookup) {
  if (is.list(x)) {
    if (is_json_class(x)) {
      x <- assign_reference(x[names(x) != "@id"],
                            specs[[get_subclass(x)]],
                            lookup)
      x <- new_json_class(lapply(x, traverse_list, specs, lookup),
                          class = get_subclass(x))
    } else {
      x <- lapply(x, traverse_list, specs, lookup)
    }
  }

  x
}
