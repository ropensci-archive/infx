
#' Make a JSON-RPC request
#'
#' Issues a POST request to a JSON-RPC server. All `@type` fields are
#' converted to/from `json_class` attributes, using [rm_json_class()] and
#' [as_json_class()]. The helper function `request_openbis()` wraps
#' `make_request()` and constructs the url the request is sent to, based on a
#' root url and an API section name (for the API section mapping, see
#' [docs](https://wiki-bsse.ethz.ch/display/openBISDoc1304/openBIS+JSON+API)).
#' As part of the JSON-RPC specification, all objects returned form the API
#' will have `@id` fields, which may be referenced if an objects is used
#' multiple times. The helper function `resolve_references()` recursively
#' resolves all references such that each object is self-contained.
#' 
#' @param url Destination url, the request is sent to.
#' @param api,host Strings used to construct the destination url.
#' @param method The API method name.
#' @param params A list structure holding the arguments which, converted to
#' JSON, will be used to call the supplied method. The `@type` entries will be
#' generated from `json_class` attributes.
#' @param version JSON-RPC protocol version to be used (defaults to `"2.0"`.
#' @param id An identifier for the JSON-RPC request (defaults to a random
#' string of length 7). Can be usually be ignored, as only single JSON-RPC
#' requests are issued per HTTP request.
#' @param x A (possibly nested) list structure for which all `@id` fields are
#' recursively removed.
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
make_request <- function(url,
                         method,
                         params,
                         version = "2.0",
                         id = paste(sample(c(letters, LETTERS, 0:9), 7),
                                    collapse = "")) {

  if (!is.list(params)) params <- list(params)

  requ <- list(id = id,
               jsonrpc = version,
               method = method,
               params = rm_json_class(params))

  resp <- httr::POST(url, body = requ, encode = "json")
  assert_that(resp$status_code == 200)

  resp$content <- jsonlite::fromJSON(rawToChar(resp$content),
                                     simplifyVector = FALSE)

  assert_that(resp$content$id == id)

  if (!is.null(resp$content$error))
    stop("Error:\n", paste(names(resp$content$error), resp$content$error,
                           sep = ": ", collapse = "\n"))

  res <- as_json_class(resp$content$result, force = TRUE)
  res <- resolve_references(res)
  res <- as_json_vec(res, force = TRUE)

  res
}

#' @rdname request
#' @export
#' 
request_openbis <- function(method,
                            params,
                            api = c("IGeneralInformationService",
                                    "IGeneralInformationChangingService",
                                    "IQueryApiServer",
                                    "IWebInformationService",
                                    "IDssServiceRpcGeneric",
                                    "IScreeningApiServer",
                                    "IDssServiceRpcScreening"),
                            host = "https://infectx.biozentrum.unibas.ch") {

  api <- match.arg(api)

  url <- switch(api,
                IGeneralInformationService =
                  "openbis/openbis/rmi-general-information-v1.json",
                IGeneralInformationChangingService =
                  "openbis/openbis/rmi-general-information-changing-v1.json",
                IQueryApiServer =
                  "openbis/openbis/rmi-query-v1.json",
                IWebInformationService =
                  "openbis/openbis/rmi-web-information-v1.json",
                IDssServiceRpcGeneric =
                  "datastore_server/rmi-dss-api-v1.json",
                IScreeningApiServer =
                  "openbis/openbis/rmi-screening-api-v1.json",
                IDssServiceRpcScreening =
                  "rmi-datastore-server-screening-api-v1.json")

  make_request(paste(host, url, sep = "/"), method, params)
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
