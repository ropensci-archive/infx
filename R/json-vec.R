
#' Construct and validate JSON object vectors
#' 
#' In order to allow method dispatch on a set of `json_class` objects without
#' resorting to iterating over the individual set members, vectors of
#' `json_class` objects are wrapped by a `json_vec` class. Iterating over
#' objects is is some cases inefficient because the openBIS API can for some
#' functions accept lists of objects. Assembling multiple `json_class` objects
#' as a list in R however breaks method dispatch, as the type of this object
#' is `list` instead of the desired `json_class` sub-class. A `json_vec` object
#' therefore represents a list of `json_class` objects of the same sub-class
#' and brings this sub-class to the surface of the compound object.
#' 
#' A `json_vec` object can be instantiated using the `json_vec()` constructor
#' which takes a list of `json_class` objects of the same sub-class. An
#' existing list of `json_class` objects can be coerced to `json_vec` using
#' `as_json_vec()`/`as.json_vec()` and applying `as_list()`/`as.list()` to a
#' `json_vec` object reverses the action of `as_json_vec()` by removing all
#' `json_vec` related class information.
#'
#' The function `is_json_vec()` and its alias `is.json_vec()` can be used to
#' test whether an object is a proper `json_vec` object. This requires that
#'   * all child elements have to be of the same sub-class
#'   * all child elements are required to be properly formed `json_class`
#'     objects
#'   * the `json_vec` class attribute has to be in last position
#'   * the remaining class attributes have to be equal to the common sub-class
#'     determined for the children.
#' 
#' Testing whether a list structure consists of `json_class` objects which are
#' of the same sub-class can be done with `has_common_subclass()`. This always
#' returns `TRUE` if a `json_class` object is passed and `FALSE` if a non-list
#' structure is passed.
#' 
#' @param ... Individual `json_class` objects, or generic compatibility
#' @param x A single/list of `json_class` object(s), or other object to coerce
#' @param simplify,.simplify Logical switch indicating whether to simplify
#' `json_vec` objects of length 1 to `json_class` objects.
#' 
#' @rdname json_vec
#' 
#' @examples
#' a <- json_class(field = "a", class = "foo")
#' b <- json_class(field = "b", class = "foo")
#'
#' ab <- json_vec(a, b)
#' 
#' print(ab)
#' 
#' identical(ab, as_json_vec(list(a, b)))
#' # as_json_vec() is idempotent
#' identical(as_json_vec(list(a, b)),
#'           as_json_vec(as_json_vec(list(a, b))))
#' 
#' # a json_class object can be turned into a json_vec of length 1
#' ab_class <- json_class(foo1 = a, foo2 = b, class = "bar")
#' length(ab_class)
#' ab_vec <- as_json_vec(ab_class)
#' length(ab_vec)
#' # this can be reversed using as_json_class()
#' identical(ab_class, as_json_class(ab_vec))
#' # this might not be desirable in all cases; the argument simplify can be
#' # used to only create json_vec objects of length greater than 1
#' identical(as_json_vec(list(a), simplify = TRUE),
#'           a)
#' 
#' # has_common_subclass() will alway return true for json_class objects
#' has_common_subclass(a)
#' # list-based objects are tested
#' has_common_subclass(list(a, b))
#' # this includes json_vec objects
#' has_common_subclass(ab)
#' # each list entry has to be a json_class object
#' has_common_subclass(list("a", "b"))
#' # here sub-classes are "foo" and "bar"
#' has_common_subclass(list(ab_class, a))
#' 
#' is_json_vec(a)
#' is_json_vec(list(a, b))
#' is_json_vec(ab)
#' 
#' @export
#' 
json_vec <- function(..., .simplify = FALSE)
  new_json_vec(list(...), simplify = .simplify)

new_json_vec <- function(x, simplify = FALSE) {

  assert_that(has_common_subclass(x))

  res <- structure(stats::setNames(x, NULL),
                   class = c(get_subclass(x), "json_vec"))

  assert_that(is_json_vec(res))

  if (simplify && length(res) == 1L)
    as_json_class(res)
  else
    res
}

#' @rdname json_vec
#' @export
#' 
as_json_vec <- function(x, ...)
  UseMethod("as_json_vec")

#' @rdname json_vec
#' @export
#' 
as.json_vec <- as_json_vec

#' @rdname json_vec
#' @export
#' 
as_json_vec.json_vec <- function(x, simplify = FALSE, ...) {

  res <- stats::setNames(x, NULL)

  if (simplify && length(res) == 1L)
    as_json_class(res)
  else
    res
}

#' @rdname json_vec
#' @export
#' 
as_json_vec.json_class <- function(x, simplify = FALSE, ...) {

  if (simplify)
    x
  else
    new_json_vec(list(x), simplify = FALSE)
}

#' @param recursive Recursively apply the function.
#' @param force Suppress error when casting an object to `json_vec` that
#' cannot be converted.
#' 
#' @rdname json_vec
#' @export
#' 
as_json_vec.list <- function(x,
                             recursive = TRUE,
                             force = FALSE,
                             simplify = FALSE,
                             ...) {

  list_to_json_vec <- function(y) {
    if (is.list(y)) {
      empty <- !as.logical(sapply(y, length))
      if (!length(y) || all(empty)) return(y)
      y <- japply(y, list_to_json_vec)
      if (is_json_class(y) || is_json_vec(y)) return(y)
      if (all(empty | sapply(y, is_json_class) | sapply(y, is_json_vec))) {
        y <- unlist(lapply(y, function(z) if (!is_json_vec(z)) list(z) else z),
                    recursive = FALSE)
        if (any(empty)) {
          y <- lapply(seq_along(y)[!empty],
                      function(i) set_attr(y[[i]], i, "original_index"))
        }
      }
      if (has_common_subclass(y)) {
        new_json_vec(y, simplify)
      } else
        y
    } else
      y
  }

  x <- x[as.logical(sapply(x, length))]

  if (!length(x))
    return(x)

  if (recursive && !is_json_vec(x))
    x <- list_to_json_vec(x)

  if (is_json_class(x) || (force && !has_common_subclass(x)))
    x
  else
    new_json_vec(x, simplify)
}

#' @rdname json_vec
#' @export
#' 
as_json_vec.default <- as_json_class.default

#' @rdname json_vec
#' @export
#' 
as.list.json_vec <- function(x, ...) {
  unclass(x)
}

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
             unlist(unique(lapply(x, get_subclass)))))
}

#' @rdname json_vec
#' @export
#' 
is.json_vec <- is_json_vec

#' @rdname json_vec
#' @export
#' 
has_common_subclass <- function(x) {

  if (!is.list(x) || !length(x))
    FALSE
  else if (is_json_class(x))
    TRUE
  else if (all(sapply(x, is_json_class) | sapply(x, is_json_vec)))
    isTRUE(length(unique(lapply(x, get_subclass))) == 1L)
  else
    FALSE
}

#' @rdname json_utils
#' @export
#' 
has_fields.json_vec <- function(x, fields, ...)
  all(sapply(x, has_fields, fields))

#' @rdname json_utils
#' @export
#' 
get_field.json_vec <- function(x, field, ...) {
  res <- lapply(x, `[[`, field)
  if (all(sapply(res, is_json_class)))
    as_json_vec(res, simplify = TRUE)
  else
    simplify2array(res, higher = FALSE)
}

#' @rdname json_utils
#' @export
#' 
has_subclass.json_vec <- function(x, class, ...)
  setequal(class, get_subclass(x))

#' @rdname json_utils
#' @export
#' 
get_subclass.json_vec <- function(x, ...)
  setdiff(class(x), "json_vec")
