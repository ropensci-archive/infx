
#' List data store servers
#'
#' `list_datastores()` lists all data store servers registered this openBIS
#' server instance and `list_datastore_urls()` returns the base urls of the
#' data store servers that host the supplied datasets (or that of the default
#' data store server in case no data set is supplied). Download urls of files
#' can be listed using the function `list_download_urls()`.
#' 
#' @inheritParams logout_openbis
#' @param data_set Set of datasets (specified as dataset codes, `DataSet` or
#' `DatasetIdentifier` objects) or NULL.
#' @param timeout Time-span (in seconds) for which the file download link
#' should be valid.
#' @param x Object representing a (set of) dataset(s), e.g. a vector of dataset
#' codes, or a set of `DataSet`s or `DatasetIdentifier`s.
#' @param path A character vector of file paths within datasets.
#' @param ... Generic compatibility.
#' 
#' @rdname list_urls
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "listDataStores")}
#' * \Sexpr{infx::docs_link("gis", "getDefaultPutDataStoreBaseURL")}
#' * \Sexpr{infx::docs_link("gis", "tryGetDataStoreBaseURL")}
#' * \Sexpr{infx::docs_link("gis", "getDataStoreBaseURLs")}
#' * \Sexpr{infx::docs_link("dsrg", "getDownloadUrlForFileForDataSet")}
#' * \Sexpr{infx::docs_link("dsrg",
#'                          "getDownloadUrlForFileForDataSetWithTimeout")}
#' 
#' @export
#' 
list_datastores <- function(token)
  request_openbis("listDataStores", token)

#' @rdname list_urls
#' @export
#' 
list_datastore_urls <- function(token, data_set = NULL) {

  if (is.null(data_set)) {

    request_openbis("getDefaultPutDataStoreBaseURL", token)

  } else {

    if (is_json_class(data_set))
      data_set <- as_json_vec(data_set)

    if (!is.character(data_set))
      data_set <- sapply(data_set, function(x) dataset_code(x))

    assert_that(is.character(data_set))

    if (length(data_set) == 1L) {

      urls <- request_openbis("tryGetDataStoreBaseURL", list(token, data_set))
      assert_that(!is.null(urls))
      stats::setNames(urls, data_set)

    } else {

      urls <- request_openbis("getDataStoreBaseURLs", list(token,
                                                           as.list(data_set)))
      res <- unlist(lapply(urls, function(url) {
        assert_that(has_subclass(url, "DataStoreURLForDataSets"),
                    has_fields(url, c("dataStoreURL", "dataSetCodes")))
        codes <- as.character(url[["dataSetCodes"]])
        stats::setNames(rep(url[["dataStoreURL"]], length(codes)), codes)
      }))

      assert_that(setequal(names(res), data_set))
      res[data_set]
    }
  }
}

#' @rdname list_urls
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

    mapply(function(a, b) {
      request_openbis("getDownloadUrlForFileForDataSet", list(token, a, b),
                      "IDssServiceRpcGeneric")
    }, x, path)

  } else {

    assert_that(is.numeric(timeout))

    mapply(function(a, b) {
      request_openbis("getDownloadUrlForFileForDataSetWithTimeout",
                      list(token, a, b, timeout),
                      "IDssServiceRpcGeneric")
    }, x, path)
  }
}

#' @rdname list_urls
#' @export
#' 
list_download_urls.DataSet <- function(token,
                                       x,
                                       path,
                                       timeout = NA,
                                       ...) {

  list_download_urls(token, dataset_code(x), path, timeout)
}

#' @rdname list_urls
#' @export
#' 
list_download_urls.DatasetIdentifier <- function(token,
                                                 x,
                                                 path,
                                                 timeout = NA,
                                                 ...) {

  list_download_urls(token, dataset_code(x), path, timeout)
}

#' @rdname list_urls
#' @export
#' 
list_download_urls.DataSetFileDTO <- function(token, x, timeout = NA, ...) {

  if (is.na(timeout)) {

    res <- sapply(as_json_vec(x), function(y) {
      request_openbis("getDownloadUrlForFileForDataSet", list(token, y),
                      "IDssServiceRpcGeneric")
    })

  } else {

    assert_that(is.numeric(timeout))

    res <- sapply(as_json_vec(x), function(y) {
      request_openbis("getDownloadUrlForFileForDataSetWithTimeout",
                      list(token, y, timeout),
                      "IDssServiceRpcGeneric")
    })
  }
}
