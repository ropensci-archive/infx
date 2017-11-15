
#' @title List plates
#'
#' @description For a login token, list all available plates.
#' 
#' @inheritParams logout_openbis
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_plates <- function(token, ...)
  query_openbis("listPlates", list(token), "IScreeningApiServer", ...)

#' @title List projects
#'
#' @description For a login token, list all available projects.
#' 
#' @inheritParams logout_openbis
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_projects <- function(token, ...)
  query_openbis("listProjects", list(token), ...)

#' @title List experiment types
#'
#' @description For a login token, list all available experiment types.
#' 
#' @inheritParams logout_openbis
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_experiment_types <- function(token, ...)
  query_openbis("listExperimentTypes", list(token), ...)

#' @title List plates
#'
#' @description For a login token, list all available plates.
#' 
#' @inheritParams logout_openbis
#' @param projects List holding Project objects for which experiments are to be
#' listed. If NULL, all experiments are returned.
#' @param exp_type Character vector, specifying the desired experiment type.
#' If NULL, all experiments are returned.
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_experiments <- function(token,
                             projects = NULL,
                             exp_type = NULL,
                             ...) {

  if (!is.null(projects)) {
    if (has_json_class(projects, "Project")) projects <- list(projects)
    assert_that(is.list(projects),
                all(sapply(projects, has_json_class, "Project")),
                length(projects) >= 1L)
  } else
    projects <- list_projects(token, ...)

  if (!is.null(exp_type)) {
    if (is.list(exp_type))
      exp_type <- sapply(exp_type, `[[`, "code")
    assert_that(is.character(exp_type),
                length(exp_type) >= 1L)
  } else
    exp_type <- sapply(list_experiment_types(token, ...), `[[`, "code")

  proj <- lapply(projects, `[`, c("spaceCode", "code"))

  res <- lapply(exp_type, function(type)
    query_openbis("listExperiments", list(token, proj, type), ...))

  do.call(c, res)
}

#' @title Get data sets for a plate
#'
#' @description Given a plate id (barcode), the corresponding sample object is
#' fetched using [get_plate_sample] and all available datasets for this sample
#' are queried.
#' 
#' @inheritParams get_plate_sample
#' 
#' @return List, containing (among others), fields \"code\", and
#' \"dataSetTypeCode\".
#' 
#' @export
#' 
list_plate_datasets <- function(token,
                                plate_id,
                                ...) {

  sample <- get_plate_sample(token, plate_id, ...)

  assert_that(has_json_class(sample, "Sample"),
              length(sample[["id"]]) == 1L)

  query_openbis("listDataSetsForSample",
                list(token,
                     sample[c("id", "permId", "identifier", "properties",
                              "retrievedFetchOptions")],
                     TRUE), ...)
}

#' @title Get data sets for a set of experiments
#'
#' @description Given a set of experiments as a data.frame, the corresponding
#' data sets are queried. Can be slow if several experiments are included in
#' the query.
#' 
#' @inheritParams logout_openbis
#' @param experiment A data.frame representing a set of experiments.
#' 
#' @return List, containing (among others), entries \"code\" and
#' \"dataSetTypeCode\".
#' 
#' @export
#' 
list_exp_datasets <- function(token,
                              experiment,
                              ...) {

  if (has_json_class(experiment, "Experiment")) experiment <- list(experiment)

  assert_that(all(sapply(experiment, has_json_class, "Experiment")),
              length(experiment) >= 1L)

  query_openbis("listDataSetsForExperiments",
                list(token,
                     lapply(experiment, `[`,
                            c("id", "permId", "identifier", "properties",
                              "experimentTypeCode")),
                     list("CHILDREN")), ...)
}

#' @title Get files for a data set
#'
#' @description Given a data set code, the corresponding files are queried.
#' 
#' @inheritParams logout_openbis
#' @param data_id Data set code.
#' @param folder Folder to which the file search is restricted.
#' 
#' @return List/data.frame, containing (among others), columns
#' \"pathInDataSet\", \"pathInListing\".
#' 
#' @export
#' 
list_files <- function(token, data_id, folder = "original", ...)
  query_openbis("listFilesForDataSet", list(token, data_id, folder, TRUE),
                "IDssServiceRpcGeneric", ...)

#' @title Get sample object of plate
#'
#' @description Given a plate id (barcode), the corresponding plate space is
#' determined using [list_plates] and the sample object representing the given
#' plate is queried.
#' 
#' @inheritParams logout_openbis
#' @param plate_id Plate barcode.
#' @param space_code The space code of the plate; it NULL, it is determined
#' automatically.
#' 
#' @return List, containing (among others), entries \"id\", \"permId\",
#' \"identifier\", \"properties\", \"retrievedFetchOptions\".
#' 
#' @export
#' 
get_plate_sample <- function(token,
                             plate_id,
                             space_code = NULL,
                             ...) {

  assert_that(is.character(plate_id), length(plate_id) == 1L)

  if (is.null(space_code)) {
    plates <- list_plates(token, ...)
    plate_match <- sapply(plates, `[[`, "plateCode") == plate_id
    assert_that(sum(plate_match) == 1L)

    space_code <- plates[[which(plate_match)]][["spaceCodeOrNull"]]
  }

  assert_that(is.character(space_code), length(space_code) == 1L)

  plate_id <- structure(list(plateCode = plate_id,
                             spaceCodeOrNull = space_code),
                        class = "json_class", json_class = "PlateIdentifier")

  query_openbis("getPlateSample", list(token, plate_id),
                "IScreeningApiServer", ...)
}
