
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

#' List property types
#'
#' Used primarily for searching, this function returns all property types
#' available throughout the queried openBis instance. As objects of several
#' types (`ControlledVocabularyPropertyType` and `PropertyType`) are returned
#' as property types by the API, the resulting object is a list with each
#' entry corresponding to a type and holding a set of object of the respective
#' type.
#' 
#' @inheritParams logout_openbis
#' @param with_relations Logical switch indicating whether relations should
#' be returned as well.
#' 
#' @export
#' 
list_property_types <- function(token, with_relations = FALSE) {

  assert_that(is.logical(with_relations), length(with_relations) == 1L)

  res <- request_openbis("listPropertyTypes", list(token, with_relations))

  classes <- sapply(res, get_subclass)

  lapply(split(res, classes), as_json_vec)
}
