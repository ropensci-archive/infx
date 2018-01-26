
#' List data store servers
#'
#' `list_datastores()` lists all data store servers registered this openBIS
#' server instance and `list_datastore_urls()` returns the base urls of the
#' data store servers that host the supplied datasets (or that of the default
#' data store server in case no data set is supplied).
#' 
#' @inheritParams logout_openbis
#' @param data_set Set of datasets (specified as dataset codes, `DataSet` or
#' `DatasetIdentifier` objects) or NULL.
#' 
#' @export
#' 
list_datastores <- function(token)
  request_openbis("listDataStores", token)

#' @rdname list_datastores
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
        assert_that(has_json_subclass(url, "DataStoreURLForDataSets"),
                    has_fields(url, c("dataStoreURL", "dataSetCodes")))
        codes <- as.character(url[["dataSetCodes"]])
        stats::setNames(rep(url[["dataStoreURL"]], length(codes)), codes)
      }))

      assert_that(setequal(names(res), data_set))
      res[data_set]
    }
  }
}
