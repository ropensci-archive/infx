
#' List datasets
#'
#' Given a login token, all available datasets are listed for the given
#' experiment(s), sample(s) or dataset code(s). Additionally it can be
#' specified whether parent or child datasets are to be included as well.
#' `list_dataset_types()` lists all available data set types on the given
#' openBIS instance and `list_dataset_id()` returns a list of dataset id
#' objects corresponding to the supplied character vector of one or more
#' dataset codes.
#' 
#' The function `list_files()` lists files associated with one or more
#' dataset(s), which can be specified with codes or `DataSet`/
#' `DatasetIdentifier` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit search for datasets/files with.
#' @param include Whether to include parent/child datasets as well.
#' @param codes Character vector of one or more dataset codes.
#' @param path A (vector of) file path(s) to be searched within a dataset.
#' @param recursive A (vector of) logicals, indicating whether to list files
#' recursively.
#' @param ... Generic compatibility.
#' 
#' @section TODO: The API function `listDataSetsForSample()` has a parameter
#' `areOnlyDirectlyConnectedIncluded`, which is currently fixed to `TRUE`. The
#' documentation contains the following explanation:
#' 
#' > If true, only data sets that are directly connected to the sample are
#' included, otherwise data sets of child samples are included as well.
#' 
#' This does however not seem to correspond to including child datasets in the
#' API call to `listDataSets()` via its `connectionsToGet` argument. As long
#' as it is not entirely clear how the inclusion of child/parent datasets
#' differs from setting `areOnlyDirectlyConnectedIncluded` to `FALSE`, this
#' option is not exposed to the user.
#' 
#' @export
#' 
list_datasets <- function(token, x, ...)
  UseMethod("list_datasets", x)

#' @rdname list_datasets
#' @export
#' 
list_datasets.Sample <- function(token,
                                 x,
                                 include = c(NA, "children", "parents", "all"),
                                 ...) {

  include <- resolve_fetch_opts(include)
  x <- remove_null(x)

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  if (length(include) == 0L) {

    if (length(x) == 1L)
      request_openbis("listDataSetsForSample", list(token, x[[1]], TRUE))
    else
      request_openbis("listDataSets", list(token, x))

  } else
    request_openbis("listDataSets", list(token, x, include))
}

#' @rdname list_datasets
#' @export
#' 
list_datasets.Experiment <- function(token,
                                     x,
                                     include = c(NA, "children", "parents",
                                                 "all"),
                                     ...) {
  x <- remove_null(x)

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  request_openbis("listDataSetsForExperiments",
                  list(token, x, resolve_fetch_opts(include)))
}

#' @rdname list_datasets
#' @export
#' 
list_datasets.character <- function(token,
                                    x,
                                    include = c(NA, "children", "parents",
                                                "all"),
                                    ...) {

  include <- resolve_fetch_opts(include)

  if (length(include) == 2L)
    request_openbis("getDataSetMetaData", list(token, as.list(x)))
  else
    request_openbis("getDataSetMetaData", list(token, as.list(x), include))
}

#' @rdname list_datasets
#' @export
#' 
list_dataset_types <- function(token)
  request_openbis("listDataSetTypes", token)

resolve_fetch_opts <- function(x = c(NA, "children", "parents", "all")) {

  x <- match.arg(x)

  if (is.na(x))
    list()
  else if (x == "all")
    list("CHILDREN", "PARENTS")
  else
    list(toupper(x))
}

#' @rdname list_datasets
#' @export
#' 
list_dataset_ids <- function(token, codes) {
  assert_that(is.character(codes))
  request_openbis("getDatasetIdentifiers", list(token, as.list(codes)),
                  "IScreeningApiServer")
}

dataset_code <- function(x, ...)
  UseMethod("dataset_code")

dataset_code.DataSet <- function(x, ...) {
  assert_that(has_fields(x, "code"))
  if (is_json_vec(x))
    sapply(x, `[[`, "code")
  else
    x[["code"]]
}

dataset_code.DatasetIdentifier <- function(x, ...) {
  assert_that(has_fields(x, "datasetCode"))
  if (is_json_vec(x))
    sapply(x, `[[`, "datasetCode")
  else
    x[["datasetCode"]]
}

#' @rdname list_datasets
#' @export
#' 
list_files <- function(token, x, ...)
  UseMethod("list_files", x)

#' @rdname list_datasets
#' @export
#' 
list_files.character <- function(token, x, path = "", recursive = TRUE) {

  max_length <- max(length(x), length(path), length(recursive))

  assert_that(length(x) == max_length || length(x) == 1L,
              length(path) == max_length || length(path) == 1L,
              length(recursive) == max_length || length(recursive) == 1L)

  x <- rep(x, max_length)
  path <- rep(path, max_length)
  recursive <- rep(recursive, max_length)

  res <- mapply(function(a, b, c) {
    request_openbis("listFilesForDataSet", list(token, a, b, c),
                    "IDssServiceRpcGeneric")
  }, x, path, recursive)

  as_json_vec(do.call(c, res))
}

#' @rdname list_datasets
#' @export
#' 
list_files.DataSet <- function(token, x, path = "", recursive = TRUE)
  list_files(token, dataset_code(x), path, recursive)

#' @rdname list_datasets
#' @export
#' 
list_files.DatasetIdentifier <- function(token, x, path = "", recursive = TRUE)
  list_files(token, dataset_code(x), path, recursive)
