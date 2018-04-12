
#' @import assertthat
NULL

#' @export
as_list <- as.list

#' OpenBIS urls
#' 
#' The helper function `api_url()` is used to create urls to InfectX
#' openBIS API endpoints and `docs_link()` generates latex links to
#' documentation pages. Both functions support the following endpoints which
#' are abbreviated as
#'   * `gis`: \Sexpr{infx::docs_link("gis")}
#'   * `gics`: \Sexpr{infx::docs_link("gics")}
#'   * `qas`: \Sexpr{infx::docs_link("qas")}
#'   * `wis`: \Sexpr{infx::docs_link("wis")}
#'   * `dsrg`: \Sexpr{infx::docs_link("dsrg")}
#'   * `sas`: \Sexpr{infx::docs_link("sas")}
#'   * `dsrs`: \Sexpr{infx::docs_link("dsrs")}
#' 
#' @param api Abreviated name of the API section (e.g. `gis` for
#' IGeneralInformationService).
#' @param host Host url.
#' 
#' @rdname openbis_urls
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
#' @param version OpenBIS version number.
#' 
#' @rdname openbis_urls
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
