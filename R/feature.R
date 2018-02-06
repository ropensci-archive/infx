
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
