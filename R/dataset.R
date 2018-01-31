
#' List datasets
#'
#' Given a login token, all available datasets are listed for the given
#' experiment(s), sample(s) or dataset code(s). Additionally it can be
#' specified whether parent or child datasets are to be included as well.
#' For the above object types, `list_datasets()` returns sets of `DataSet`
#' objects. If `list_datasets()` is dispatched on plate objects (`Plate`,
#' `PlateIdentifier` or `PlateMetadata`), `ImageDatasetReference` objects are
#' returned. Similarly, if `MaterialIdentifierScreening` objects are used
#' as input, `PlateWellReferenceWithDatasets` objects are returned, which each
#' contain `ImageDatasetReference` and `FeatureVectorDatasetReference` objects.
#' Both `ImageDatasetReference` and `FeatureVectorDatasetReference` implement
#' the `IDatasetIdentifier` interface, just as `DatasetIdentifier` does.
#' 
#' `list_dataset_types()` lists all available data set types on the given
#' openBIS instance and `list_dataset_id()` returns a list of dataset id
#' objects corresponding to the supplied character vector of one or more
#' dataset codes or set of `DataSet` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit search for datasets/files with.
#' @param include Whether to include parent/child datasets as well.
#' @param experiment When searching for datasets associated with materials,
#' the search can be limited to a single experiment.
#' @param type For listing image datasets, it can be specified, whether only
#' raw image datasets, only segmentation image datasets or any kind of image
#' datasets (default) are to be listed.
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

list_img_ds <- function(token, x, type = c(NA, "raw", "segmentation"), ...) {

  type <- match.arg(type)

  fun <- switch(match.arg(type),
                `NA` = "listImageDatasets",
                raw = "listRawImageDatasets",
                segmentation = "listSegmentationImageDatasets")

  request_openbis(fun, list(token, as_json_vec(x)), "IScreeningApiServer")
}

#' @rdname list_datasets
#' @export
#' 
list_datasets.PlateIdentifier <- list_img_ds

#' @rdname list_datasets
#' @export
#' 
list_datasets.Plate <- list_img_ds

#' @rdname list_datasets
#' @export
#' 
list_datasets.PlateMetadata <- list_img_ds

#' @rdname list_datasets
#' @export
#' 
list_datasets.MaterialIdentifierScreening <- function(token,
                                                      x,
                                                      experiment = NULL,
                                                      ...) {

  list_plate_well_ref(token, x, experiment, include_datasets = TRUE)
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
list_dataset_ids <- function(token, x, ...)
  UseMethod("list_dataset_ids", x)

#' @rdname list_datasets
#' @export
#' 
list_dataset_ids.character <- function(token, x, ...)
  request_openbis("getDatasetIdentifiers", list(token, as.list(x)),
                  "IScreeningApiServer")

#' @rdname list_datasets
#' @export
#' 
list_dataset_ids.DataSet <- function(token, x, ...)
  list_dataset_ids(token, dataset_code(x))

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
