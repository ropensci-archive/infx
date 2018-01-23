
#' List datasets
#'
#' Given a login token, all available datasets are listed for the given
#' experiment(s) or sample(s). Additionally it can be specified whether parent
#' or child datasets are to be included as well.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit search for datasets with.
#' @param include Whether to include parent/child datasets as well.
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

  include <- match.arg(include)
  x <- remove_null(x)

  if (is.na(include)) {

    if (!is_json_vec(x))
      request_openbis("listDataSetsForSample", list(token, x, TRUE))
    else
      request_openbis("listDataSets", list(token, x))

  } else {

    if (include == "all")
      include <- list("CHILDREN", "PARENTS")
    else
      include <- list(toupper(include))

    if (!is_json_vec(x))
      x <- as_json_vec(x)

    request_openbis("listDataSets", list(token, x, include))
  }
}

#' @rdname list_datasets
#' @export
#' 
list_datasets.Experiment <- function(token,
                                     x,
                                     include = c(NA, "children", "parents",
                                                 "all"),
                                     ...) {

  include <- match.arg(include)
  x <- remove_null(x)

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  if (is.na(include))
    include <- list()
  else if (include == "all")
    include <- list("CHILDREN", "PARENTS")
  else
    include <- list(toupper(include))

  request_openbis("listDataSetsForExperiments", list(token, x, include))
}

#' List dataset types
#'
#' Given a login token, all available dataset types are listed.
#' 
#' @inheritParams logout_openbis
#' 
#' @return A a list of DataSetType objects.
#' 
#' @export
#' 
list_dataset_types <- function(token)
  request_openbis("listDataSetTypes", token)
