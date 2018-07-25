
#' List projects
#'
#' A project forms one of the most basic entities in the organizational
#' hierarchy of openBIS. Each project consists of one or several experiments
#' and one or more projects are contained in each space, which is the topmost
#' structure used for grouping experiments. For InfectX, each project
#' corresponds to a separate pathogen. All registered projects visible to
#' the current user can be listed by calling the function `list_projects()`.
#' 
#' @inheritParams logout_openbis
#' @param ... Further arguments will be passed to [make_requests()].
#' 
#' @family object listing functions
#' 
#' @section openBIS:
#' * \Sexpr[results=rd]{infx::docs_link("gis", "listProjects")}
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis()
#' 
#'   proj <- list_projects(tok)
#'   length(proj)
#'   get_field(proj, "code")
#' 
#'   logout_openbis(tok)
#' }
#' 
#' @export
#' 
list_projects <- function(token, ...)
  make_request(api_url("gis", attr(token, "host_url"), ...),
               "listProjects",
               list(token),
               ...)
