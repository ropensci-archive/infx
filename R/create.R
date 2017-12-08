
#' @title Create a plate id object
#'
#' @description PlateIdentifier objects are used throughout the openBis API
#' to identify plates. In addition to the plate barcode, the space the plate
#' resides in is needed. If unknown (NULL), this property can be determined by
#' listing all Plates (using [list_plates]) and matching the provided barcode.
#' If several plates in different spaces have the same barcode, an error is
#' thrown.
#' 
#' @inheritParams logout_openbis
#' @param plate_id Plate barcode.
#' @param space_code The space code of the plate; it NULL, it is determined
#' automatically.
#' 
#' @return A single PlateIdentifier object as a list of type json_class.
#' 
#' @export
#' 
create_plate_id <- function(plate_id,
                            space_code = NULL,
                            token = NULL) {

  assert_that(is.character(plate_id), length(plate_id) == 1L)

  if (is.null(space_code)) {
    assert_that(!is.null(token))
    plates <- list_plates(token)
    plate_match <- sapply(plates, `[[`, "plateCode") == plate_id
    assert_that(sum(plate_match) == 1L)

    space_code <- plates[[which(plate_match)]][["spaceCodeOrNull"]]
  }

  assert_that(is.character(space_code), length(space_code) == 1L)

  as_json_class(c(`@type` = "PlateIdentifier",
                  list(plateCode = plate_id, spaceCodeOrNull = space_code)))
}

#' @title Create experiment id objects
#'
#' @description Either an ExperimentIdentifier object is created by specifying
#' all needed information or, if some information is missing, openBis is
#' queried for all available ExperimentIdentifiers and the provided regular
#' expressions are used to select a subset of all ids.
#' 
#' @inheritParams logout_openbis
#' @param exp_code,proj_code,space_code,perm_id If all are not NULL, character
#' vectors of length 1 that make up the returned ExperimentIdentifier object
#' or if one or more is NULL, regular expressions to match fields against the
#' full list of available ExperimentIdentifier objects.
#' @param ... Passed to [grepl].
#' 
#' @return A single or a list of ExperimentIdentifier object(s).
#' 
#' @export
#' 
create_exp_ids <- function(exp_code = NULL,
                           proj_code = NULL,
                           space_code = NULL,
                           perm_id = NULL,
                           token = NULL,
                           ...) {

  params <- list(permId = perm_id,
                 spaceCode = space_code,
                 projectCode = proj_code,
                 experimentCode = exp_code)

  if (any(sapply(params, is.null))) {

    assert_that(!is.null(token))
    all_exps <- list_experiment_ids(token)

    hits <- lapply(names(params), function(key, ...) {
      if (is.null(params[[key]]))
        rep(TRUE, length(all_exps))
      else
        grepl(params[[key]], sapply(all_exps, `[[`, key), ...)
    }, ...)

    hits <- Reduce(`&`, hits)

    assert_that(sum(hits) >= 1L)

    if (sum(hits) == 1L)
      all_exps[[which(hits)]]
    else
      all_exps[hits]

  } else {

    assert_that(all(sapply(params, length) == 1L))

    as_json_class(c(`@type` = "ExperimentIdentifier", params))
  }
}

#' @title Create data set id objects
#'
#' @description From a list of objects, each containing a datasetCode field or
#' from a character vector of datasetCodes, create a list of DatasetIdentifier
#' objects.
#' 
#' @inheritParams logout_openbis
#' @param data_set Either a single/list of json_class object(s), each
#' containing a datasetCode field or a character vector.
#' 
#' @return A single or a list of DatasetIdentifier object(s).
#' 
#' @export
#' 
create_dataset_id <- function(token, data_set) {

  if (is_json_class(data_set)) {
    data_set <- data_set[["datasetCode"]]
  } else if (all(sapply(data_set, is_json_class))) {
    codes <- sapply(data_set, `[[`, "datasetCode")
    assert_that(length(codes) == length(data_set))
    data_set <- codes
  }

  assert_that(is.character(data_set), length(data_set) >= 1L)
  query_openbis("getDatasetIdentifiers",
                list(token, lapply(data_set, identity)),
                "IScreeningApiServer")
}