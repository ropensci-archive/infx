
#' Make a JSON-RPC request
#'
#' Issues a POST request to a JSON-RPC server. All `@type` fields are
#' converted to/from `json_class` attributes, using [as_json_list()] and
#' [as_json_class()]. The helper function `request_openbis()` wraps
#' `make_request()` and constructs the url the request is sent to.
#' 
#' @param url Destination url, the request is sent to.
#' @param api, host Strings used to construct the destination url.
#' @param method The API method name.
#' @param params A list structure holding the arguments which, converted to
#' JSON, will be used to call the supplied method. The `@type` entries will be
#' generated from `json_class` attributes.
#' @param version JSON-RPC protocol version to be used (defaults to `"2.0"`.
#' @param id An identifier for the JSON-RPC request (defaults to `"1"`). Can be
#' ignored, as only single JSON-RPC requests are issued per HTTP request.
#' 
#' @return A (nested) list holding the response from the JSON-RPC server
#' (`@type` entries are converted to `json_class` attributes).
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
make_request <- function(url,
                         method,
                         params,
                         version = "2.0",
                         id = "1") {

  if (!is.list(params)) params <- list(params)

  req <- list(id = id,
              jsonrpc = version,
              method = method,
              params = as_json_list(params))

  res <- httr::POST(url, body = req, encode = "json")

  assert_that(res$status_code == 200)

  res$content <- jsonlite::fromJSON(rawToChar(res$content),
                                    simplifyVector = FALSE)

  if (!is.null(res$content$error))
    stop("Error:\n", paste(names(res$content$error), res$content$error,
                           sep = ": ", collapse = "\n"))

  as_json_class(res$content$result)
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
