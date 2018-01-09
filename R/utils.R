
#' Helper for S3 default error
#'
#' Whenever the default function of an S3 generic is a `not implemented` error,
#' this function can be used. It is a wrapper around [base::stop()], with a
#' custom error message.
#' 
#' @param x The object on which the dispatch is done.
#' @param prefix Optional string that is inserted before the error message.
#' 
#' @keywords internal
#' 
error_default <- function(x, prefix = "") {
  stop(prefix, "cannot handle objects of type c(",
       paste0("\"", class(x), "\"", collapse = ", "), ").")
}