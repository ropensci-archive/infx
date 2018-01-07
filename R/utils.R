
#' Helper for S3 default error
#'
#' Whenever the default function of an S3 generic is a `not implemented` error,
#' tis function can be used.
#' 
#' @param x The object on which the dispatch is done.
#' 
#' @keywords internal
#' 
error_default <- function(x, ...) {
  stop("cannot handle objects of type c(",
       paste0("\"", class(x), "\"", collapse = ", "), ").")
}