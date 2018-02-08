
#' List datasets
#'
#' Given a login token, all available datasets are listed for the given
#' experiment(s), sample(s) or dataset code(s). Additionally it can be
#' specified whether parent or child datasets are to be included as well.
#' For the above object types, `list_datasets()` returns sets of `DataSet`
#' objects. Dataset id objects can be listed using `list_dataset_id()`.
#' 
#' Several classes in addition to `DatasetIdentifier` implement the
#' `IDatasetIdentifier` interface, including
#' 
#'   * `DatasetReference`
#'   * `FeatureVectorDatasetReference`
#'   * `FeatureVectorDatasetWellReference`
#'   * `ImageDatasetReference`
#'   * `MicroscopyImageReference`
#'   * `PlateImageReference`
#' 
#' The above objects are returned by `list_references()` and the exact return
#' type depends on the argument types. If `list_references()` is dispatched on
#' plate objects (`Plate`, `PlateIdentifier` or `PlateMetadata`),
#' `ImageDatasetReference` objects are returned (except if the type argument
#' is set to `feature`, in which case, `FeatureVectorDatasetReference`
#' object(s) are returned). Similarly, if `MaterialIdentifierScreening`
#' objects are used as input, `PlateWellReferenceWithDatasets` objects are
#' returned, which each contain `ImageDatasetReference` and
#' `FeatureVectorDatasetReference` objects.
#' 
#' Whenever `list_references()` is dispatched on (a) dataset id(s) or dataset
#' reference object(s), the resulting object type depends on whether a (set of)
#' `WellPosition` object(s) were specified as `wells` argument. For its
#' default value (NULL), a set of `MicroscopyImageReference` objects is
#' returned, while `PlateImageReference` objects are returned otherwise.  
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
#' @param wells A (set of) `WellPosition` object(s) to limit the dataset
#' listing to.
#' @param channels A character vector with imaging channel names to limit the
#' dataset listing to.
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

#' @rdname list_datasets
#' @export
#' 
list_references <- function(token, x, ...)
  UseMethod("list_references", x)

list_img_ds <- function(token,
                        x,
                        type = c(NA, "raw", "segmentation", "feature"),
                        ...) {

  type <- match.arg(type)

  fun <- switch(match.arg(type),
                `NA` = "listImageDatasets",
                raw = "listRawImageDatasets",
                segmentation = "listSegmentationImageDatasets",
                feature = "listFeatureVectorDatasets")

  request_openbis(fun, list(token, as_json_vec(x)), "IScreeningApiServer")
}

#' @rdname list_datasets
#' @export
#' 
list_references.PlateIdentifier <- list_img_ds

#' @rdname list_datasets
#' @export
#' 
list_references.Plate <- list_img_ds

#' @rdname list_datasets
#' @export
#' 
list_references.PlateMetadata <- list_img_ds

#' @rdname list_datasets
#' @export
#' 
list_references.MaterialIdentifierScreening <- function(token,
                                                        x,
                                                        experiment = NULL,
                                                        ...) {

  list_plate_well_ref(token, x, experiment, include_datasets = TRUE)
}

list_img_ref_wrapper <- function(token, x, wells = NULL, channels, ...)
  list_img_ref(token, x, wells, channels)

#' @rdname list_datasets
#' @export
#' 
list_references.DatasetIdentifier <- list_img_ref_wrapper

#' @rdname list_datasets
#' @export
#' 
list_references.DataSet <- function(token, x, wells = NULL, channels, ...)
  list_references(token, list_dataset_ids(token, x), wells, channels, ...)

#' @rdname list_datasets
#' @export
#' 
list_references.DatasetReference <- list_img_ref_wrapper

#' @rdname list_datasets
#' @export
#' 
list_references.FeatureVectorDatasetReference <- list_img_ref_wrapper

#' @rdname list_datasets
#' @export
#' 
list_references.FeatureVectorDatasetWellReference <- list_img_ref_wrapper

#' @rdname list_datasets
#' @export
#' 
list_references.ImageDatasetReference <- list_img_ref_wrapper

#' @rdname list_datasets
#' @export
#' 
list_references.MicroscopyImageReference <- list_img_ref_wrapper

#' @rdname list_datasets
#' @export
#' 
list_references.PlateImageReference <- list_img_ref_wrapper

#' List image references
#' 
#' Used for double dispatching on the `list_datasets()` generic, list image
#' reference objects either for a specific (set of) `WellPosition` object(s)
#' or for the specified datasets in general.
#' 
#' @inheritParams list_datasets
#' 
#' @keywords internal
#' 
#' @export
#' 
list_img_ref <- function(token, x, wells = NULL, channels, ...)
  UseMethod("list_img_ref", wells)

#' @rdname list_img_ref
#' @keywords internal
#' @export
#' 
list_img_ref.NULL <- function(token, x, wells, channels, ...) {

  x <- as_json_vec(x)

  if (length(channels) > 1L)
    channels <- as.list(channels)

  res <- lapply(x, function(z)
    request_openbis("listImageReferences", list(token, z, channels),
                    "IDssServiceRpcScreening"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_img_ref
#' @keywords internal
#' @export
#' 
list_img_ref.WellPosition <- function(token, x, wells, channels, ...) {

  x <- as_json_vec(x)
  wells <- as_json_vec(wells)

  if (length(channels) > 1L)
    channels <- as.list(channels)

  res <- lapply(x, function(z)
    request_openbis("listPlateImageReferences",
                    list(token, z, wells, channels),
                    "IDssServiceRpcScreening"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_datasets
#' @export
#' 
list_dataset_types <- function(token)
  request_openbis("listDataSetTypes", token)

#' Extract dataset code
#' 
#' Given a (set of) dataset object(s), for each one extract the dataset code
#' and return a character vector of codes.
#' 
#' @param x Dataset object(s).
#' @param ... Generic compatibility.
#' 
#' @keywords internal
#' @export
#' 
dataset_code <- function(x, ...)
  UseMethod("dataset_code")

#' @rdname dataset_code
#' @keywords internal
#' @export
#' 
dataset_code.DataSet <- function(x, ...) {
  assert_that(has_fields(x, "code"))
  if (is_json_vec(x))
    sapply(x, `[[`, "code")
  else
    x[["code"]]
}

#' @rdname dataset_code
#' @keywords internal
#' @export
#' 
dataset_code.DatasetIdentifier <- function(x, ...) {
  assert_that(has_fields(x, "datasetCode"))
  if (is_json_vec(x))
    sapply(x, `[[`, "datasetCode")
  else
    x[["datasetCode"]]
}

resolve_fetch_opts <- function(x = c(NA, "children", "parents", "all")) {

  x <- match.arg(x)

  if (is.na(x))
    list()
  else if (x == "all")
    list("CHILDREN", "PARENTS")
  else
    list(toupper(x))
}
