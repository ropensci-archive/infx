
#' List data store servers and urls
#' 
#' In order to download files from openBIS, download urls have to be generated
#' first, which can be done by calling `list_download_urls()`. This function
#' is used in [fetch_files()], which iterates over the selected files, creating
#' download links and executing the downloads. All data store servers
#' registered to an openBIS instance are listed by `list_datastores()` and data
#' store server urls per data set can be queried by calling
#' `list_datastore_urls()`.
#' 
#' To specify files for which links are requested by `list_download_urls()`,
#' both a data set code and a file path are required. Objects, apart from
#' character vectors of data set codes, that may be passed to identify the
#' data set therefore include `DataSet`, `DatasetIdentifier`,
#' `DatasetReference`, `FeatureVectorDatasetReference`,
#' `FeatureVectorDatasetWellReference`, `ImageDatasetReference`,
#' `MicroscopyImageReference` and `PlateImageReference`. Additionally, dispatch
#' of `list_download_urls()` is possible on `DataSetFileDTO` objects which
#' contain both information on data set and file path of a file. A `timeout`
#' argument may be specified, determining how long (in seconds) the generated
#' url is valid for. If no specific timeout value is passed the url is valid
#' for what the openBIS documentation calls "a short time".
#' 
#' `list_datastore_urls()` as `list_download_urls()` ultimately requires a
#' character vector of data set codes to make the API call and therefore
#' dispatch is possible on, in addition to character vector, `DataSet`,
#' `DatasetIdentifier`, `DatasetReference`, `FeatureVectorDatasetReference`,
#' `FeatureVectorDatasetWellReference`, `ImageDatasetReference`,
#' `MicroscopyImageReference` and `PlateImageReference` objects. Dispatch on
#' `NULL` requests the default data store server url. Data store sever url
#' related functionality is uninteresting for the InfectX set-up, as only a
#' single data store server exists, the url of which can be retrieved by a call
#' to `list_datastores()`.
#'
#' @inheritParams logout_openbis
#' @param x Object representing a (set of) dataset(s), e.g. a vector of dataset
#' codes, or a set of `DataSet`s or `DatasetIdentifier`s.
#' @param path A character vector of file paths within datasets.
#' @param timeout Time-span (in seconds) for which the file download link
#' should be valid.
#' @param ... Generic compatibility. Extra arguments will be passed to
#' [make_requests()].
#' 
#' @rdname list_urls
#' 
#' @family resource listing/downloading functions
#' 
#' @section openBIS:
#' * \Sexpr[results=rd]{infx::docs_link("dsrg",
#'                      "getDownloadUrlForFileForDataSet")}
#' * \Sexpr[results=rd]{infx::docs_link("dsrg",
#'                      "getDownloadUrlForFileForDataSetWithTimeout")}
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis()
#'   
#'   # data store server information
#'   list_datastores(tok)
#' 
#'   # search for a cell profiler feature data set from plate KB2-03-1I
#'   search <- search_criteria(
#'     attribute_clause("type", "HCS_ANALYSIS_CELL_FEATURES_CC_MAT"),
#'     sub_criteria = search_sub_criteria(
#'       search_criteria(attribute_clause("code",
#'                                        "/INFECTX_PUBLISHED/KB2-03-1I")),
#'       type = "sample"
#'     )
#'   )
#'   ds <- search_openbis(tok, search)
#' 
#'   # list all files of this data set
#'   files <- list_files(tok, ds)
#'   # extract file paths
#'   file_paths <- get_field(files, "pathInDataSet")
#'   # select a file
#'   file_path <- file_paths[grepl("Count_Cells", file_paths)]
#' 
#'   # generate url
#'   list_download_urls(tok, ds, file_path)
#' 
#'   # generate url and download file
#'   dat <- read_mat_files(url(list_download_urls(tok, ds, file_path)[[1L]]))
#'   attributes(dat)
#'   str(as.integer(dat))
#' 
#'   # set timeout to 2 sec
#'   file_url <- list_download_urls(tok, ds, file_path, timeout = 2L)
#'   tmp <- read_mat_files(url(file_url[[1L]]))
#' 
#'   # let timeout expire
#'   file_url <- list_download_urls(tok, ds, file_path, timeout = 2L)
#'   Sys.sleep(3L)
#'   tmp <- read_mat_files(url(file_url[[1L]]))
#' 
#'   logout_openbis(tok)
#' }
#' @export
#' 
list_download_urls <- function(token, x, ...)
  UseMethod("list_download_urls", x)

#' @rdname list_urls
#' @export
#' 
list_download_urls.character <- function(token,
                                         x,
                                         path,
                                         timeout = NA,
                                         ...) {

  assert_that(is.character(path))

  max_length <- max(length(x), length(path))

  if (max_length > 1L) {

    if (length(x) == 1L)
      x <- rep(x, max_length)

    if (length(path) == 1L)
      path <- rep(path, max_length)

    assert_that(length(x) == length(path))
  }

  if (is.na(timeout)) {
    fun <- "getDownloadUrlForFileForDataSet"
    params <- Map(function(a, b) list(token, a, b), x, path)

  } else {

    assert_that(is.numeric(timeout))

    fun <- "getDownloadUrlForFileForDataSetWithTimeout"
    params <- Map(function(a, b) list(token, a, b, timeout), x, path)
  }

  res <- make_requests(api_url("dsrg", attr(token, "host_url"), ...),
                       fun,
                       params,
                       ...)

  Map(function(y, a, b) {
    set_attr(set_attr(y, a, "data_set"), b, "path")
  }, res, x, path)
}

list_dl_url <- function(token, x, path, timeout = NA, ...)
  list_download_urls(token, dataset_code(x), path, timeout, ...)

#' @rdname list_urls
#' @export
#' 
list_download_urls.DataSet <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.DatasetIdentifier <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.DatasetReference <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.FeatureVectorDatasetReference <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.FeatureVectorDatasetWellReference <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.ImageDatasetReference <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.MicroscopyImageReference <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.PlateImageReference <- list_dl_url

#' @rdname list_urls
#' @export
#' 
list_download_urls.DataSetFileDTO <- function(token, x, timeout = NA, ...) {

  x <- as_json_vec(x)

  if (is.na(timeout)) {

    fun <- "getDownloadUrlForFileForDataSet"
    params <- lapply(x, function(y) list(token, y))

  } else {

    assert_that(is.numeric(timeout))

    fun <- "getDownloadUrlForFileForDataSetWithTimeout"
    params <- lapply(x, function(y) list(token, y, timeout))
  }
  
  res <- make_requests(api_url("dsrg", attr(token, "host_url"), ...),
                       fun,
                       params,
                       ...)

  Map(set_attr, res, x, MoreArgs = list(attr_name = "ds_file"))
}

#' @rdname list_urls
#' @section openBIS:
#' * \Sexpr[results=rd]{infx::docs_link("gis", "listDataStores")}
#' @export
#' 
list_datastores <- function(token, ...)
  make_request(api_url("gis", attr(token, "host_url"), ...),
               "listDataStores",
               list(token),
               ...)

#' @rdname list_urls
#' @section openBIS:
#' * \Sexpr[results=rd]{infx::docs_link("gis",
#'                      "getDefaultPutDataStoreBaseURL")}
#' * \Sexpr[results=rd]{infx::docs_link("gis", "tryGetDataStoreBaseURL")}
#' * \Sexpr[results=rd]{infx::docs_link("gis", "getDataStoreBaseURLs")}
#' @export
#' 
list_datastore_urls <- function(token, x = NULL, ...)
  UseMethod("list_datastore_urls", x)

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.NULL <- function(token, x, ...)
  make_request(api_url("gis", attr(token, "host_url"), ...),
               "getDefaultPutDataStoreBaseURL",
               list(token),
               ...)

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.character <- function(token, x, ...) {

    if (length(x) == 1L) {

      urls <- make_request(api_url("gis", attr(token, "host_url"), ...),
                           "tryGetDataStoreBaseURL",
                           list(token, x),
                           ...)

      assert_that(!is.null(urls))
      stats::setNames(urls, x)

    } else {

      urls <- make_request(api_url("gis", attr(token, "host_url"), ...),
                           "getDataStoreBaseURLs",
                           list(token, as.list(x)),
                           ...)
      urls <- as_json_vec(urls)

      res <- unlist(lapply(urls, function(url) {
        codes <- as.character(get_field(url, "dataSetCodes"))
        stats::setNames(rep(get_field(url, "dataStoreURL"), length(codes)),
                        codes)
      }))

      assert_that(setequal(names(res), x))
      res[x]
    }
}

list_ds_urls <- function(token, x, ...)
  list_datastore_urls(token, dataset_code(x), ...)

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.DataSet <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.DatasetIdentifier <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.DatasetReference <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.FeatureVectorDatasetReference <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.FeatureVectorDatasetWellReference <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.ImageDatasetReference <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.MicroscopyImageReference <- list_ds_urls

#' @rdname list_urls
#' @export
#' 
list_datastore_urls.PlateImageReference <- list_ds_urls


#' OpenBIS urls
#' 
#' The helper function `api_url()` is used to create urls to InfectX
#' openBIS API endpoints and `docs_link()` generates latex links to
#' documentation pages. Both functions support the following endpoints which
#' are abbreviated as
#'   * `gis`: \Sexpr[results=rd]{infx::docs_link("gis")}
#'   * `gics`: \Sexpr[results=rd]{infx::docs_link("gics")}
#'   * `qas`: \Sexpr[results=rd]{infx::docs_link("qas")}
#'   * `wis`: \Sexpr[results=rd]{infx::docs_link("wis")}
#'   * `dsrg`: \Sexpr[results=rd]{infx::docs_link("dsrg")}
#'   * `sas`: \Sexpr[results=rd]{infx::docs_link("sas")}
#'   * `dsrs`: \Sexpr[results=rd]{infx::docs_link("dsrs")}
#' 
#' If for some reason an url is desired from `api_url()` that cannot be
#' constructed by pasting `host_url` and one of the hard-coded API endpoints
#' together, this can be passed as `full_url`, which will simply be returned.
#' 
#' @param api_endpoint Abbreviated name of the API section (e.g. `gis` for
#' IGeneralInformationService).
#' @param host_url Host url.
#' @param full_url Instead of constructing the API endpoint url, a string can
#' be passed which will be returned again.
#' @param ... Further arguments are ignored.
#' 
#' @rdname openbis_urls
#' 
#' @family utility functions
#' 
#' @examples
#' # default endpoint is the GeneralInformationService interface
#' api_url()
#' # base url can be customized
#' api_url(host_url = "https://foobar.xyz")
#' # ScreeningApiServer interface endpoint
#' api_url("sas")
#' # manual url
#' api_url(full_url = "https://foobar.xyz/openbis/new-api-section-v1.json")
#' 
#' # link to GeneralInformationService interface docs
#' docs_link()
#' # add a method name (only to the link text)
#' docs_link(method_name = "foo_bar")
#' # link to ScreeningApiServer interface docs
#' docs_link("sas")
#' # link to most recent version of docs
#' docs_link("sas", version = "16.05.6")
#' 
#' @export
#' 
api_url <- function(api_endpoint = c("gis", "gics", "qas", "wis", "dsrg",
                                     "sas", "dsrs"),
                    host_url = "https://infectx.biozentrum.unibas.ch",
                    full_url = NULL,
                    ...) {

  if (!is.null(full_url)) {
    assert_that(is.string(full_url))
    return(full_url)
  }

  assert_that(is.string(host_url))

  url <- switch(match.arg(api_endpoint),
                gis = "openbis/openbis/rmi-general-information-v1.json",
                gics = paste0("openbis/openbis/",
                              "rmi-general-information-changing-v1.json"),
                qas = "openbis/openbis/rmi-query-v1.json",
                wis = "openbis/openbis/rmi-web-information-v1.json",
                dsrg = "datastore_server/rmi-dss-api-v1.json",
                sas = "openbis/openbis/rmi-screening-api-v1.json",
                dsrs = "rmi-datastore-server-screening-api-v1.json")

  paste(host_url, url, sep = "/")
}

#' @param method_name Name of the method for which the link is created.
#' @param version OpenBIS version number.
#' 
#' @rdname openbis_urls
#' @export
#' 
docs_link <- function(api_endpoint = c("gis", "gics", "qas", "wis", "dsrg",
                                       "sas", "dsrs"),
                      method_name = NULL,
                      version = "13.04.0") {

  api_endpoint <- match.arg(api_endpoint)

  url <- switch(api_endpoint,
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

  txt <- switch(api_endpoint,
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
