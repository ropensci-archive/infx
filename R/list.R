
#' List projects
#'
#' List all projects available on the queries openBIS instance. A project forms
#' one of the most basic entities in the organizational hierarchy of openBIS.
#' One or more projects are contained in each space (the topmost
#' organizational entity) and each project consists of one or several
#' experiments.
#' 
#' @inheritParams logout_openbis
#' 
#' @export
#' 
list_projects <- function(token)
  request_openbis("listProjects", token)

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
