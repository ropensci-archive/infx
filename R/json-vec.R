#' Construct a json_vec object
#'
#' `new_json_vec` is a low-level constructor that takes a list of
#' `json_class` objects of the same sub-class. `json_vec` constructs a json_vec
#' from individual json_class objects and `as.json_vec` is a S3 generic that
#' converts existing objects.
#'
#' @param ... Individual `json_class` objects
#' @param x A single/list of `json_class` object(s), or other object to coerce
#' 
#' @examples
#' a <- structure("a", class = c("foo", "json_class"))
#' b <- structure("b", class = c("foo", "json_class"))
#'
#' new_json_vec(list(a, b))
#' json_vec(a, b)
#' 
#' as.json_vec(list(a, b))
#' 
#' @export
#' 
json_vec <- function(...) {
  new_json_vec(list(...))
}

#' @rdname json_vec
#' 
#' @export
#' 
new_json_vec <- function(x) {

  if (is_json_class(x)) {
    sub_class <- get_json_subclass(x)
    x <- list(x)
  } else if (all(sapply(x, is_json_class))) {
    assert_that(has_common_subclass(x))
    sub_class <- get_common_subclass(x)
  } else
    stop("expecting a single json_class object or a list thereof.")

  structure(x, class = c(sub_class, "json_vec"))
}

#' @rdname json_vec
#' @export
#' 
as.json_vec <- function(x, ...) {
  UseMethod("as.json_vec")
}

#' @rdname json_vec
#' @export
#' 
as.json_vec.json_vec <- function(x, ...) {
  x
}

#' @rdname json_vec
#' @export
#' 
as.json_vec.json_class <- function(x, ...) {
  new_json_vec(x)
}

#' @rdname json_vec
#' @export
#' 
as.json_vec.list <- function(x, ...) {
  new_json_vec(x)
}

#' @rdname json_vec
#' @export
#' 
as.json_vec.default <- function(x, ...) error_default(x)

#' @rdname json_vec
#' @export
#' 
as.list.json_vec <- function(x, ...) {
  unclass(x)
}

#' @rdname json_vec
#' 
#' @export
#' 
has_common_subclass <- function(x) {

  if (is_json_class(x)) {
    TRUE
  } else if (all(sapply(x, is_json_class))) {
    all_classes <- lapply(x, get_json_subclass)
    isTRUE(length(unique(all_classes)) == 1L)
  } else
    FALSE
}

#' @rdname json_vec
#' @export
#' 
get_common_subclass <- function(x, ...) {
  UseMethod("get_common_subclass")
}

#' @rdname json_vec
#' @export
#' 
get_common_subclass.json_class <- function(x, ...) {
  get_json_subclass(x)
}

#' @rdname json_vec
#' @export
#' 
get_common_subclass.list <- function(x, ...) {
  assert_that(has_common_subclass(x))
  unlist(unique(lapply(x, get_json_subclass)))
}

#' @rdname json_vec
#' @export
#' 
get_common_subclass.json_vec <- function(x, ...) {
  class <- setdiff(class(x), "json_vec")
  assert_that(class == get_common_subclass.list(x))
  class
}

#' @rdname json_vec
#' @export
#' 
get_common_subclass.default <- function(x, ...) error_default(x)
