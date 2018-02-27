
#' List features
#'
#' For a given set of feature vector data sets, `list_features()` provides the
#' list of all available features. This contains the code, label and
#' description of the feature, as (set of) `FeatureInformation` object(s). If
#' for different data sets different sets of features are available,
#' `list_features()` provides the union of the features of all data sets.
#' 
#' Similarly, `list_feature_codes()` provides the list of all available
#' features as character vector or feature codes. If for different data sets
#' different sets of features are available, `list_feature_codes()` provides
#' the union of the features of all data sets.
#' 
#' @inheritParams logout_openbis
#' @param x Object to specify the set of feature vector data sets of interest.
#' @param ... Generic compatibility 
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "listAvailableFeatures")}
#' @export
#' 
list_features <- function(token, x, ...)
    UseMethod("list_features", x)

list_feats <- function(token, x, ...)
  request_openbis("listAvailableFeatures",
                  list(token, as_json_vec(remove_null(x))),
                  "IDssServiceRpcScreening")

#' @rdname list_features
#' @export
#' 
list_features.FeatureVectorDatasetReference <- list_feats

#' @rdname list_features
#' @export
#' 
list_features.FeatureVectorDatasetWellReference <- list_feats

#' @rdname list_features
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "listAvailableFeatureCodes")}
#' @export
#' 
list_feature_codes <- function(token, x, ...)
    UseMethod("list_feature_codes", x)

list_feat_codes <- function(token, x, ...)
  request_openbis("listAvailableFeatureCodes",
                  list(token, as_json_vec(remove_null(x))),
                  "IDssServiceRpcScreening")

#' @rdname list_features
#' @export
#' 
list_feature_codes.FeatureVectorDatasetReference <- list_feat_codes

#' @rdname list_features
#' @export
#' 
list_feature_codes.FeatureVectorDatasetWellReference <- list_feat_codes

#' Fetch feature data
#'
#' For a given set of feature vector data sets, `fetch_features()` fetches
#' feature data for the specified feature codes, or for all available features
#' in case the argument `feature_codes` is not specified (or `NA`). If
#' for different data sets different sets of features are available,
#' the union of the features of all data sets is searched for. The returned
#' object is of type `FeatureVectorDataset`, which for each entry contains a
#' `FeatureVectorDatasetReference` and a set of `FeatureVector`(s), one for
#' each well.
#' 
#' If only a limited set of wells is of interest, the search can be limited by
#' dispatching `fetch_features()` on a (set of)
#' `FeatureVectorDatasetWellReference` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to specify the set of feature vector data sets of interest.
#' @param feature_codes A character vector of feature codes or NA (all
#' available feature codes).
#' @param ... Generic compatibility
#' 
#' @section TODO: Even though there exists a constructor for
#' `FeatureVectorDatasetWellReference` objects, which takes two arguments, one
#' for the corresponding `FeatureVectorDatasetReference` object and one for
#' a `WellPosition` objects, this does not work. Furthermore, class information
#' cannot be supplied as this will cause an error as well (hence the use of
#' `rm_json_class()`). Why the function `loadFeaturesForDatasetWellReferences`
#' behaves this way is currently unclear.
#' 
#' @export
#' 
fetch_features <- function(token, x, feature_codes = NA, ...)
    UseMethod("fetch_features", x)

#' @rdname fetch_features
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "loadFeatures")}
#' @export
#' 
fetch_features.FeatureVectorDatasetReference <- function(token,
                                                         x,
                                                         feature_codes = NA,
                                                         ...) {
  x <- as_json_vec(remove_null(x))

  if (length(feature_codes) == 1L && is.na(feature_codes)) {
    feature_codes <- list_feature_codes(token, x)
  } else {
    assert_that(is.character(feature_codes))
    feature_codes <- as.list(feature_codes)
  }

  request_openbis("loadFeatures", list(token, x, feature_codes),
                  "IDssServiceRpcScreening")
}

#' @rdname fetch_features
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "loadFeaturesForDatasetWellReferences")}
#' @export
#' 
fetch_features.FeatureVectorDatasetWellReference <- function(
  token,
  x,
  feature_codes = NA,
  ...) {

  x <- as_json_vec(x)

  fields <- c("datasetCode", "datastoreServerUrl", "plate",
              "experimentIdentifier", "plateGeometry", "registrationDate",
              "properties", "wellPosition")

  assert_that(has_fields(x, fields))

  if (length(feature_codes) == 1L && is.na(feature_codes)) {
    feature_codes <- list_feature_codes(token, x)
  } else {
    assert_that(is.character(feature_codes))
    feature_codes <- as.list(feature_codes)
  }

  x <- lapply(lapply(x, `[`, fields), rm_json_class, recursive = FALSE,
              restore_type = FALSE)

  request_openbis("loadFeaturesForDatasetWellReferences",
                  list(token, x, feature_codes),
                  "IDssServiceRpcScreening")
}
