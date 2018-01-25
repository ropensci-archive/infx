#' List data store servers
#'
#' List all data store servers registered this openBIS server instance.
#' 
#' @inheritParams logout_openbis
#' 
#' @export
#' 
list_datastores <- function(token)
  request_openbis("listDataStores", token)
