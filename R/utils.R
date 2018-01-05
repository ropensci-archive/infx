
#' @title Helper querying openBis API
#'
#' @description Issues a POST request to the JSON-RPC based openBis API v1,
#' using [make_request]. Documentation is available at
#' \code{https://wiki-bsse.ethz.ch/display/openBISDoc/openBIS+JSON+API}.
#' 
#' @inheritParams make_request
#' @param api The api section the used method is part of. This is used to
#' construct the url the request is sent to.
#' @param host The base url, the request is sent to.
#' 
#' @return A list/data.frame holding the response from openBis.
#' 
query_openbis <- function(method,
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

#' @title Make a JSON-RPC request
#'
#' @description Issues a POST request to a JSON-RPC server. All "@type" fields
#' are converted to/from "json_class" attributes.
#' 
#' @param url Url, the request is sent to.
#' @param method The method name
#' @param params A list structure holding the arguments which, converted to
#' JSON, will be used to call the supplied method. The "@type" entries will be
#' generated from "json_class" attributes.
#' @param version JSON-RPC protocol version to be used.
#' @param id Id of the JSON-RPC request.
#' 
#' @return A (nested) list holding the response from the JSON-RPC server
#' ("@type" entries are converted to "json_class" attributes).
#' 
make_request <- function(url,
                         method,
                         params,
                         version = "2.0",
                         id = "1") {

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
