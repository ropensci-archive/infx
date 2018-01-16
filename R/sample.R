
#' List sample types
#'
#' Given a login token, all available sample types are listed.
#' 
#' @inheritParams logout_openbis
#' 
#' @return A a list of SampleType objects.
#' 
#' @export
#' 
list_sample_types <- function(token)
  request_openbis("listSampleTypes", token)
