
#' Make a JSON-RPC request
#'
#' The function `make_requests()` and a wrapper for single requests
#' (`make_request()`) issues one or several POST request to the specified
#' JSON-RPC server. The urls for the various openBIS endpoints can be
#' constructed using the `api_url()` function. If several requests are issued,
#' these can be run asynchronously using `do_request_parallel()` or serially
#' using `do_request_serial()`. For both functions, a number of retries can be
#' specified and a function can be supplied that will be run on the returned
#' data if the request returns successfully.
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
#' 
#' @rdname request
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#'   projects <- request_openbis("listProjects", tok)
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

  assert_that(length(bodies) == length(urls))

  do_request_serial(urls, bodies, ...)
}

#' @param ... Further arguments to `make_request` are passed to
#' `make_requests` and from `make_requests` to `do_request_serial`.
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
#' @param done A function that is applied to the result of a successful
#' request.
#' 
#' @rdname request
#' @export
#' 
do_request_serial <- function(urls,
                              bodies,
                              n_try = 2L,
                              done = process_json) {

  res <- vector("list", length(urls))

  repeat {

    to_do <- sapply(res, is.null)
    if (sum(to_do) == 0L)
      break

    n_try <- n_try - 1L
    if (n_try < 0L)
      stop("data could not be fetched successfully.")

    res[to_do] <- mapply(function(url, body) {
      body_raw <- charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))

      handle <- curl::new_handle(post = TRUE,
                                 postfieldsize = length(body_raw),
                                 postfields = body_raw)
      handle <- curl::handle_setheaders(handle,
                                        "Content-Type" = "application/json")

      resp <- curl::curl_fetch_memory(url, handle = handle)

      if (resp$status_code != 200)
        NULL
      else {
        resp <- jsonlite::fromJSON(rawToChar(resp$content),
                                   simplifyVector = FALSE)
        assert_that(resp$id == body$id)

        if (!is.null(resp$error)) {
          data <- resp$error$data[!grepl("^@", names(resp$error$data))]
          warning("\nError with code ", resp$error$code, ":\n",
                  paste(strwrap(paste(names(data), data, sep = ": "),
                                indent = 2L, exdent = 4L), collapse = "\n"))
          NULL
        } else {
          resp <- done(resp$result)
          assert_that(!is.null(resp))
          resp
        }
      }
    }, urls[to_do], bodies[to_do], SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  assert_that(all(sapply(res, Negate(is.null))))

  res
}

#' @param api,host Strings used to construct the destination url.
#' 
#' @rdname request
#' @export
#' 
api_url <- function(api = c("gis", "gics", "qas", "wis", "dsrg", "sas",
                            "dsrs"),
                    host = "https://infectx.biozentrum.unibas.ch") {

  url <- switch(match.arg(api),
                gis = "openbis/openbis/rmi-general-information-v1.json",
                gics = paste0("openbis/openbis/",
                              "rmi-general-information-changing-v1.json"),
                qas = "openbis/openbis/rmi-query-v1.json",
                wis = "openbis/openbis/rmi-web-information-v1.json",
                dsrg = "datastore_server/rmi-dss-api-v1.json",
                sas = "openbis/openbis/rmi-screening-api-v1.json",
                dsrs = "rmi-datastore-server-screening-api-v1.json")

  paste(host, url, sep = "/")
}

#' @param method_name Name of the method for which the link is created.
#' 
#' @rdname request
#' @export
#' 
docs_link <- function(api = c("gis", "gics", "qas", "wis", "dsrg", "sas",
                              "dsrs"),
                      method_name = NULL,
                      version = "13.04.0") {

  api <- match.arg(api)

  url <- switch(api,
                gis = paste0("generic/shared/api/v1/",
                             "IGeneralInformationService.html"),
                gics = paste0("generic/shared/api/v1/",
                              "IGeneralInformationChangingService.html"),
                qas = "plugin/query/shared/api/v1/IQueryApiServer.html",
                wis = "generic/shared/api/v1/IWebInformationService.html",
                dsrg = paste0("dss/generic/shared/api/v1/",
                              "IDssServiceRpcGeneric.html"),
                sas = paste0("plugin/screening/shared/api/v1/",
                             "IScreeningApiServer.html"),
                dsrs = paste0("dss/screening/shared/api/v1/",
                              "IDssServiceRpcScreening.html"))

  url <- paste("https://svnsis.ethz.ch/doc/openbis", version,
               "ch/systemsx/cisd/openbis", url, sep = "/")

  txt <- switch(api,
                gis = "IGeneralInformationService",
                gics = "IGeneralInformationChangingService",
                qas = "IQueryApiServer",
                wis = "IWebInformationService",
                dsrg = "IDssServiceRpcGeneric",
                sas = "IScreeningApiServer",
                dsrs = "IDssServiceRpcScreening")

  if (!is.null(method_name))
    txt <- paste(txt, method_name, sep = ":")

  paste0("\\href{", url, "}{", txt, "}")
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
