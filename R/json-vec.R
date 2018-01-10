
#' Construct a json_vec object
#'
#' `new_json_vec()` is a low-level constructor that takes a list of
#' `json_class` objects of the same sub-class. `json_vec()` constructs a
#' json_vec from individual json_class objects and `as_json_vec()`/
#' `as.json_vec()` is an S3 generic function that converts existing objects.
#' Applying `as.list()` to a `json_vec` object reverses the action of
#' `new_json_vec()` by removing all json vec related class information.
#' Finally, `is_json_vec()`/`is.json_vec()` test whether an object is a proper
#' `json_vec` object, which entails that
#'   * all child elements have to be of the same sub-class
#'   * all child elements are required to be properly formed `json_class`
#'     objects
#'   * the `json_vec` class attribute has to be in last position
#'   * the remaining class attributes have to be equal to the common sub-class
#'     determined for the children.
#'
#' @param ... Individual `json_class` objects, or generic compatibility
#' @param x A single/list of `json_class` object(s), or other object to coerce
#' 
#' @examples
#' a <- structure(list("a"), class = c("foo", "json_class"))
#' b <- structure(list("b"), class = c("foo", "json_class"))
#'
#' new_json_vec(list(a, b))
#' json_vec(a, b)
#' 
#' as_json_vec(list(a, b))
#' as_json_vec(a)
#' 
#' is_json_vec(json_vec(a, b))
#' is_json_vec(a)
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

  assert_that(has_common_subclass(x))

  res <- structure(x, class = c(get_common_subclass(x), "json_vec"))

  assert_that(is_json_vec(res))

  res
}

#' @rdname json_vec
#' @export
#' 
as_json_vec <- function(x, ...) {
  UseMethod("as_json_vec")
}

#' @rdname json_vec
#' @export
#' 
as.json_vec <- as_json_vec

#' @rdname json_vec
#' @export
#' 
as_json_vec.json_vec <- function(x, ...) {
  x
}

#' @rdname json_vec
#' @export
#' 
as_json_vec.json_class <- function(x, ...) {
  new_json_vec(list(x))
}

#' @rdname json_vec
#' @export
#' 
as_json_vec.list <- function(x, ...) {
  new_json_vec(x)
}

#' @rdname json_vec
#' @export
#' 
as_json_vec.default <- function(x, ...) error_default(x)

#' @export
as.list.json_vec <- function(x, ...) {
  unclass(x)
}

#' @export
`[.json_vec` <- function(x, i, ...) {
  new_json_vec(NextMethod())
}

#' @export
`[<-.json_vec` <- function(x, i, ..., value) {

  sub_class <- get_common_subclass(x)

  assert_that(get_common_subclass(value) == sub_class)

  if (is_json_class(value))
    value <- list(value)

  NextMethod()
}

#' @export
`[<-.json_vec` <- function(x, i, ..., value) {

  sub_class <- get_common_subclass(x)

  assert_that(get_common_subclass(value) == sub_class)

  if (is_json_class(value))
    value <- list(value)

  NextMethod()
}

#' @export
`[[<-.json_vec` <- function(x, i, ..., value) {

  assert_that(get_json_subclass(value) == get_common_subclass(x))

  NextMethod()
}

#' @rdname json_vec
#' 
#' @export
#' 
has_common_subclass <- function(x) {

  if (is_json_class(x))
    TRUE
  else if (all(sapply(x, is_json_class)))
    isTRUE(length(unique(lapply(x, get_json_subclass))) == 1L)
  else
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
  assert_that(is_json_vec(x))
  setdiff(class(x), "json_vec")
}

#' @rdname json_vec
#' @export
#' 
get_common_subclass.default <- function(x, ...) error_default(x)

#' @rdname json_vec
#' @export
#' 
is_json_vec <- function(x) {

  isTRUE(inherits(x, "json_vec") &&
         utils::tail(class(x), 1) == "json_vec" &&
         length(class(x)) > 1L &&
         all(sapply(x, is_json_class)) &&
         has_common_subclass(x) &&
         all(setdiff(class(x), "json_vec") ==
             unlist(unique(lapply(x, get_json_subclass)))))
}

#' @rdname json_vec
#' @export
#' 
is.json_vec <- is_json_vec
