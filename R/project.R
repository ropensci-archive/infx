
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
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "listProjects")}
#' 
#' @export
#' 
list_projects <- function(token)
  request_openbis("listProjects", token)
