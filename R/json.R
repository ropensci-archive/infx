
#' @title Create/destroy JSON class
#'
#' @description To communicate object type information via JSON to the
#' Jackson-powered openBis interface, the "@type" field is used. Furthermore,
#' the "@id" field is used by the json-rpc specification to map requests in
#' async scenarios. Data received from openBis is stripped of both "@type" and
#' "@id" and the type information is saved as "json_class" attribute. Such
#' objects also have the class "json_class" added. The opposite action removes
#' both the "json_class" class and the "json_class" attribute and writes the
#' "json_class" information to an "@type" filed. Both actions are recursively
#' applied to lists.
#' 
#' @rdname json_class
#' 
#' @param x Object to process.
#' @param mode Whether to move the type information from "@type" field to
#' "json_class" attribute ("add") of the other way around ("rm").
#' 
#' @return The modified object used as input.
#' 
#' @export
#' 
json_class <- function(x, mode = c("add", "rm")) {

  mode <- match.arg(mode)

  if (mode == "add" && "@type" %in% names(x)) {

    assert_that(sum("@type" == names(x)) == 1L)

    x <- structure(x[!names(x) %in% c("@type", "@id")],
                   class = c(class(x), "json_class"),
                   json_class = x[["@type"]])

  } else if (mode == "rm" && has_json_class(x)) {

    x <- c(`@type` = attr(x, "json_class"), x)
    class(x) <- class(x)[class(x) != "json_class"]
    attr(x, "json_class") <- NULL
  }

  sublist <- sapply(x, is.list)
  if (any(sublist))
    x[sublist] <- lapply(x[sublist], json_class, mode)

  x
}

#' @rdname json_class
#' @export
#' 
add_json_class <- function(x) json_class(x, "add")

#' @rdname json_class
#' @export
#' 
rm_json_class <- function(x) json_class(x, "rm")

#' @title Test if object has a JSON class
#'
#' @description Either tests whether an object has any JSON class attached or
#' a specific one.
#' 
#' @rdname json_class
#' 
#' @param class (Optional) class name to test.
#' 
#' @return A single logical.
#' 
#' @export
#' 
has_json_class <- function(x, class = NULL) {
  if (is.null(class)) !is.null(attr(x, "json_class"))
  else {
    assert_that(is.character(class), length(class) == 1L)
    if (is.null(attr(x, "json_class"))) FALSE
    else attr(x, "json_class") == class
  }
}

#' @title Test if object represents a JSON class
#'
#' @description Tests whether an object inherits from "json_class".
#' 
#' @rdname json_class
#' 
#' @return A single logical.
#' 
#' @export
#' 
is_json_class <- function(x) inherits(x, "json_class")

#' @title Subset a JSON object
#'
#' @description Custom sub-setting of JSON objects that preserve class and
#' "json_class" attributes. This is useful when objects are created from
#' openBIS results which are subsequently used in further queries, but the
#' constructors they are passed to require only a subset of the fetched fields.
#' 
#' @rdname json_class
#' 
#' @param i Sub-setting information.
#' @param ... Generic compatibility.
#' 
#' @return The subsetted object.
#' 
#' @export
#' 
`[.json_class` <- function(x, i, ...) {
  r <- NextMethod("[")
  attr(r, "json_class") <- attr(x, "json_class")
  class(r) <- class(x)
  r
}