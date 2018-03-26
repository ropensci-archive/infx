
#' List and download feature data
#' 
#' In openBIS, features are datasets that are treated differently from
#' generic datasets. Briefly put, tabular datasets where columns correspond
#' to features and rows to wells can be marked as feature datasets which makes
#' it possible to query openBIS for individual feature values for selected
#' wells instead of having to download the entire table for a plate. The
#' relevant object types for handling feature data are
#' `FeatureVectorDatasetReference` and `FeatureVectorDatasetWellReference`,
#' where the former represents feature data at plate level and the latter at
#' well level. The function `list_features()` can be used to enumerate
#' available features and `fetch_features()` will download feature data.
#' 
#' For a given set of feature vector datasets, `list_features()` provides the
#' list of all available features. This contains the code, label and
#' description of the feature, as (set of) `FeatureInformation` object(s). If
#' for different datasets different sets of features are available,
#' `list_features()` provides the union of the features of all datasets.
#' 
#' Similarly, `list_feature_codes()` provides the list of all available
#' features as character vector or feature codes. If for different datasets
#' different sets of features are available, `list_feature_codes()` provides
#' the union of the features of all datasets.
#' 
#' For a given set of feature vector datasets, `fetch_features()` fetches
#' feature data for the specified feature codes, or for all available features
#' in case the argument `feature_codes` is not specified (or `NA`). If
#' for different datasets different sets of features are available,
#' the union of the features of all datasets is searched for. The returned
#' object is of type `FeatureVectorDataset`, which for each entry contains a
#' `FeatureVectorDatasetReference` and a set of `FeatureVector`(s), one for
#' each well.
#' 
#' If only a limited set of wells is of interest, the search can be limited by
#' dispatching `fetch_features()` on a (set of)
#' `FeatureVectorDatasetWellReference` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to specify the set of feature vector datasets of interest.
#' @param ... Generic compatibility
#' 
#' @rdname list_fetch_features
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "listAvailableFeatures")}
#' @export
#' 
list_features <- function(token, x, ...)
    UseMethod("list_features", x)

list_feats <- function(token, x, ...)
  make_request(api_url("dsrs"), "listAvailableFeatures",
               list(token, as_json_vec(remove_null(x))))

#' @rdname list_fetch_features
#' @export
#' 
list_features.FeatureVectorDatasetReference <- function(token,
                                                        x,
                                                        wells = NULL,
                                                        ...) {
  if (is.null(wells))
    list_feats(token, x)
  else
    list_feats(token, feat_ds_well_ref(x, wells))
}

#' @rdname list_fetch_features
#' @export
#' 
list_features.FeatureVectorDatasetWellReference <- list_feats

#' @rdname list_fetch_features
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "listAvailableFeatureCodes")}
#' @export
#' 
list_feature_codes <- function(token, x, ...)
    UseMethod("list_feature_codes", x)

list_feat_codes <- function(token, x, ...)
  make_request(api_url("dsrs"), "listAvailableFeatureCodes",
               list(token, as_json_vec(remove_null(x))))

#' @rdname list_fetch_features
#' @export
#' 
list_feature_codes.FeatureVectorDatasetReference <- function(token,
                                                             x,
                                                             wells = NULL,
                                                             ...) {
  if (is.null(wells))
    list_feat_codes(token, x)
  else
    list_feat_codes(token, feat_ds_well_ref(x, wells))
}

#' @rdname list_fetch_features
#' @export
#' 
list_feature_codes.FeatureVectorDatasetWellReference <- list_feat_codes

#' @rdname list_fetch_features
#' 
#' @param feature_codes A character vector of feature codes or NA (all
#' available feature codes).
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

#' @rdname list_fetch_features
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "loadFeatures")}
#' @export
#' 
fetch_features.FeatureVectorDatasetReference <- function(token,
                                                         x,
                                                         feature_codes = NA,
                                                         wells = NULL,
                                                         ...) {

  if (is.null(wells)) {
    x <- as_json_vec(remove_null(x))

    if (length(feature_codes) == 1L && is.na(feature_codes)) {
      feature_codes <- list_feature_codes(token, x)
    } else {
      assert_that(is.character(feature_codes))
      feature_codes <- as.list(feature_codes)
    }

    make_request(api_url("dsrs"), "loadFeatures",
                 list(token, x, feature_codes))
  } else {
    fetch_features(token, feat_ds_well_ref(x, wells), feature_codes)
  }
}

#' @rdname list_fetch_features
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

  make_request(api_url("dsrs"), "loadFeaturesForDatasetWellReferences",
               list(token, x, feature_codes))
}

#' Create well feature dataset reference objects
#' 
#' While `FeatureVectorDatasetReference` objects represent feature datasets on
#' a plate level, `FeatureVectorDatasetWellReference` reference feature
#' datasets on well-level. In order to create such well-level representations,
#' a set of `FeatureVectorDatasetReference` and a set of `WellPosition` are
#' combined to `FeatureVectorDatasetWellReference` objects where each instance
#' contains a single object of the inputted sets. If either argument is of
#' length greater than 1, the other argument has to be of the same length or of
#' length 1, in which case it will be [base:rep()]eated to the required length.
#' 
#' @param x A set of `FeatureVectorDatasetReference` objects.
#' @param wells A set of `WellPosition` objects.
#' 
#' @noRd
#' 
#' @export
#' 
feat_ds_well_ref <- function(x, wells) {

  x <- as_json_vec(x)
  wells <- as_json_vec(wells)

  max_len <- max(length(x), length(wells))

  if (max_len > 1L) {
    if (length(x) == 1L)
      x <- rep(x, max_len)
    if (length(wells) == 1L)
      wells <- rep(wells, max_len)
  }

  fields <- c("datasetCode", "datastoreServerUrl", "plate",
              "experimentIdentifier", "plateGeometry", "registrationDate",
              "properties")

  assert_that(has_subclass(x, "FeatureVectorDatasetReference"),
              has_subclass(wells, "WellPosition"),
              length(x) == length(wells),
              has_fields(x, fields))

  as_json_vec(
    Map(function(ref, well) {
      new_json_class(c(
        as.list(ref[fields], keep_asis = FALSE, recursive = FALSE,
                restore_type = FALSE),
        list(wellPosition = well)
      ), class = "FeatureVectorDatasetWellReference")
    }, x, wells)
  )
}