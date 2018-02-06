
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
list_files_old <- function(token, data_id, folder = "original")
  request_openbis("listFilesForDataSet", list(token, data_id, folder, TRUE),
                  "IDssServiceRpcGeneric")
