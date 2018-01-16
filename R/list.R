
#' @title List plates
#'
#' @description For a login token, list all available plates.
#' 
#' @inheritParams logout_openbis
#' @param exp_id A single ExperimentIdentifier object to limit the plate
#' search to.
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_plates <- function(token, exp_id = NULL) {
  if (is.null(exp_id))
    request_openbis("listPlates", list(token), "IScreeningApiServer")
  else{
    assert_that(has_json_subclass(exp_id, "ExperimentIdentifier"))
    request_openbis("listPlates", list(token, exp_id), "IScreeningApiServer")
  }
}

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
list_projects <- function(token)
  request_openbis("listProjects", list(token))

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
list_experiment_ids <- function(token)
  request_openbis("listExperiments", list(token), "IScreeningApiServer")

#' @title List experiments
#'
#' @description For a login token (and optionally a set of projects and
#' experiment types), list all available experiments.
#' 
#' @inheritParams logout_openbis
#' @param projects List holding Project objects for which experiments are to be
#' listed. If NULL, all experiments are returned.
#' @param exp_type Character vector, specifying the desired experiment type.
#' If NULL, all experiments are returned.
#' @param exp_ids A set of experiment ID objects
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_experiments <- function(token,
                             projects = NULL,
                             exp_type = NULL,
                             exp_ids = NULL) {

  if (!is.null(exp_ids)) {
    if (has_json_subclass(exp_ids, "ExperimentIdentifier"))
      exp_ids <- paste0("/", exp_ids[c("spaceCode", "projectCode",
                                       "experimentCode")],
                        collapse = "")
    else if (all(sapply(exp_ids, has_json_subclass, "ExperimentIdentifier")))
      exp_ids <- lapply(exp_ids, function(x)
        paste0("/", x[c("spaceCode", "projectCode", "experimentCode")],
               collapse = ""))

    if (is.character(exp_ids) && length(exp_ids) == 1L)
      exp_ids <- list(exp_ids)
    else if (is.character(exp_ids) && length(exp_ids) > 1L)
      exp_ids <- lapply(exp_ids, identity)

    assert_that(all(sapply(exp_ids, is.character)),
                all(sapply(exp_ids, length)  == 1L),
                length(exp_ids) >= 1L)

    if (!is.null(projects) || !is.null(exp_type))
      warning("ignoring params projects/exp_type.")

    request_openbis("listExperiments", list(token, exp_ids))

  } else {

    if (!is.null(projects)) {
      if (has_json_subclass(projects, "Project")) projects <- list(projects)
      assert_that(is.list(projects),
                  all(sapply(projects, has_json_subclass, "Project")),
                  length(projects) >= 1L)
    } else
      projects <- list_projects(token)

    if (!is.null(exp_type)) {
      if (is.list(exp_type))
        exp_type <- sapply(exp_type, `[[`, "code")
      assert_that(is.character(exp_type),
                  length(exp_type) >= 1L)
    } else
      exp_type <- sapply(list_experiment_types(token), `[[`, "code")

    proj <- lapply(projects, `[`, c("spaceCode", "code"))

    res <- lapply(exp_type, function(type)
      request_openbis("listExperiments", list(token, proj, type)))

    res <- res[!sapply(res, is.null)]

    do.call(c, res)
  }
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
                                plate_id) {

  sample <- get_plate_sample(token, plate_id)

  assert_that(length(sample) == 1L)

  sample <- sample[[1]]
  assert_that(has_json_subclass(sample, "Sample"))

  request_openbis("listDataSetsForSample",
                  list(token,
                       sample[c("id", "permId", "identifier", "properties",
                                "retrievedFetchOptions")],
                       TRUE))
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
                              experiment) {

  if (has_json_subclass(experiment, "Experiment"))
    experiment <- list(experiment)

  assert_that(all(sapply(experiment, has_json_subclass, "Experiment")),
              length(experiment) >= 1L)

  request_openbis("listDataSetsForExperiments",
                  list(token,
                       lapply(experiment, `[`,
                              c("id", "permId", "identifier", "properties",
                                "experimentTypeCode")),
                       list("CHILDREN")))
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
list_files <- function(token, data_id, folder = "original")
  request_openbis("listFilesForDataSet", list(token, data_id, folder, TRUE),
                  "IDssServiceRpcGeneric")

#' @title Get sample object of plate
#'
#' @description Given a plate id (barcode), the corresponding plate space is
#' determined using [list_plates] and the sample object representing the given
#' plate is queried.
#' 
#' @inheritParams create_plate_id
#' 
#' @return List, containing (among others), entries \"id\", \"permId\",
#' \"identifier\", \"properties\", \"retrievedFetchOptions\".
#' 
#' @export
#' 
get_plate_sample <- function(token, plate_id, space_code = NULL)
  request_openbis("getPlateSample",
                  list(token,
                       create_plate_id(plate_id, space_code, token)),
                  "IScreeningApiServer")
