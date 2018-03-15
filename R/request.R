
#' Make a JSON-RPC request
#'
#' The function `make_requests()` (and a wrapper for single requests,
#' `make_request()`) issues one or several JSON-RPC request(s) to the specified
#' server. Urls can be either passed as a character vector or a list of calls
#' which will be evaluated using [base::eval()]. The urls for the various
#' openBIS endpoints can be constructed using the [api_url()] function.
#' 
#' If several requests are issued, these can be run asynchronously using
#' `do_requests_parallel()` or serially using `do_requests_serial()`, which
#' is controlled by the argument `n_con`, specifying the number of allowed
#' simultaneous connections. The arguments `methods` and `params`, as well as
#' the optional arguments `ids` and `version` are assembled into a list of
#' JSON-RPC request objects which are subsequently passed to `do_requests_*()`
#' along with all further arguments passed as `...`. These include `n_try`,
#' specifying a maximum number of allowed retries for failed requests, as well
#' as `create_handle`, `check` and `finally`, which can be used to modify the
#' behavior of `do_requests_*()`.
#' 
#' All `@type` fields are converted to/from `json_class` attributes, using
#' [rm_json_class()] and [as_json_class()]. Furthermore, as part of the
#' JSON-RPC specification, all objects returned form the API will have `@id`
#' fields, which may be referenced if an objects is used multiple times. The
#' helper function `resolve_references()` recursively resolves all references
#' such that each object is self-contained.
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
    do_requests_parallel(urls, bodies, n_con, ...)
  else
    do_requests_serial(urls, bodies, ...)
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
#' @param finally A function that is applied to the `result` entry of the list
#' returned by the `check` function.
#' 
#' @rdname request
#' @export
#' 
do_requests_serial <- function(urls,
                               bodies,
                               n_try = 1L,
                               create_handle = create_request_handle,
                               check = check_request_result,
                               finally = process_json) {

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

    if (is.null(res)) {
      add_request(i, tries - 1L)
    } else {
      assert_that(is.list(res), "result" %in% names(res))
      res <- finally(res$result)
      if (length(urls) > 1L)
        pb$tick(1L)
      res
    }
  }

  assert_that(is.character(urls) || all(sapply(urls, is.call)),
              is.list(bodies),
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

#' @rdname request
#' @export
#' 
do_requests_parallel <- function(urls,
                                 bodies,
                                 n_con = 5L,
                                 n_try = 1L,
                                 create_handle = create_request_handle,
                                 check = check_request_result,
                                 finally = process_json) {

  add_request <- function(i, tries) {

    if (tries == 0L) {
      warning("could not carry out request within ", n_try, " tries.")
      return(invisible(NULL))
    }

    curl::curl_fetch_multi(
      url = eval(urls[[i]]),
      handle = create_handle(bodies[[i]]),
      pool = pool,
      done = function(x) {

        resp <- check(x, bodies[[i]])

        if (is.null(resp)) {
          add_request(i, tries - 1L)
        } else {
          assert_that(is.list(resp), "result" %in% names(resp))
          res[[i]] <<- finally(resp$result)
          if (length(urls) > 1L)
            pb$tick(1L)
        }
      },
      fail = function(x) add_request(i, tries - 1L)
    )
  }

  assert_that(is.character(urls) || all(sapply(urls, is.call)),
              is.list(bodies),
              length(urls) == length(bodies),
              is.count(n_con),
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

  res <- vector("list", length(urls))

  pool <- curl::new_pool(host_con = n_con)

  for (j in seq_along(urls))
    add_request(j, n_try)

  curl::multi_run(pool = pool)

  res
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
    NULL

  } else {

    resp <- jsonlite::fromJSON(rawToChar(resp$content),
                               simplifyVector = FALSE)
    assert_that(resp$id == body$id)

    if (!is.null(resp$error)) {

      data <- resp$error$data[!grepl("^@", names(resp$error$data))]
      warning("\nerror with code ", resp$error$code, ":\n",
              paste(strwrap(paste(names(data), data, sep = ": "),
                            indent = 2L, exdent = 4L), collapse = "\n"))
      NULL

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
  if (is.null(x)) {
    warning("an api call returned NULL.")
    x <- list()
  }
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
