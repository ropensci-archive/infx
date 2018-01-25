
#' List projects
#'
#' List all projects available on the queried openBIS instance. A project forms
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
