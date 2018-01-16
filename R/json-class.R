
#' Create JSON class objects
#'
#' To communicate object type information via JSON to the Jackson-powered
#' openBis interface, the `@type` field is used. Data received from openBis is
#' recursively stripped of `@type` fields and the type information is saved as
#' class attribute. Such objects also have the class `json_class` added. The
#' function `as_json_class()` (or its alias `as.json_class()`) powers this
#' recursive conversion of a list with filed `@type` into a `json_class`
#' object. The constructor `json_class()` is available for non-recursive
#' instantiation of `json_class` objects.
#' 
#' The action of `as_json_class()` is reversed by `rm_json_class()`. This
#' removes both the `json_class` class attribute and the JSON class attribute
#' itself, which is subsequently written to a `@type` filed. This preserving
#' of type information can be disabled, by setting the argument `restore_type`
#' to `FALSE`. Furthermore, the action can be applied recursively with the
#' argument `recursive`. The function `as.list()` can also be used to perform
#' the above actions, but with default arguments, it does nothing, as
#' functions such as `base::sapply()` and `base::lapply()`, call `as.list()`.
#' 
#' JSON class objects have custom sub-setting and printing functions available.
#' Sub-setting of JSON objects that preserve class and `json_class`
#' attributes. This is useful when objects are created from openBIS results
#' which are subsequently used in further queries, but the constructors they
#' are passed to require only a subset of the fetched fields.
#' 
#' @param x Object to process.
#' @param class JSON sub-class name.
#' @param force Suppress error when casting an object to `json_class` that
#' cannot be converted.
#' @param recursive Recursively apply the function.
#' @param restore_type When removing the `json_class` information from an
#' object, whether to preserve the subclass attribute as `@type` filed.
#' @param keep_asis Used in `as.list()`, if `TRUE`, the `json_class` object
#' is returned as-is, if `FALSE`, class attributes may be dropped/written to
#' the list structure into the `@type` field.
#' @param ... Generic compatibility.
#' 
#' @rdname json_class_create
#'  
#' @examples
#' lst <- list(`@type` = "foo", "a", "b")
#' cls <- as_json_class(lst)
#' 
#' print(cls)
#' is_json_class(cls)
#' get_json_subclass(cls)
#' 
#' identical(rm_json_class(cls), lst)
#' 
#' @export
#' 
json_class <- function(..., class) {

  assert_that(!missing(class))

  x <- list(...)

  new_json_class(x, class)
}

new_json_class <- function(x, class = NULL) {

  assert_that(is.list(x))

  if (is.null(class)) {
    assert_that(sum("@type" == names(x)) == 1L)
    class <- x[["@type"]]
    x <- x[names(x) != "@type"]
  }

  assert_that(is.character(class),
              length(class) == 1L)

  structure(x, class = c(class, "json_class"))
}

#' @rdname json_class_create
#' @export
#' 
as_json_class <- function(x, ...) {
  UseMethod("as_json_class")
}

#' @rdname json_class_create
#' @export
#' 
as.json_class <- as_json_class

#' @rdname json_class_create
#' @export
#' 
as_json_class.json_class <- function(x, ...) {
  x
}

#' @rdname json_class_create
#' @export
#' 
as_json_class.list <- function(x, ...) {

  if (length(x) == 0L)
    return(NULL)

  x <- lapply(x, as_json_class, force = TRUE)
  if ("@type" %in% names(x))
    new_json_class(x)
  else
    x
}

#' @rdname json_class_create
#' @export
#' 
as_json_class.json_vec <- function(x, ...) {
  assert_that(is_json_vec(x),
              length(x) == 1L)
  x[[1]]
}

#' @rdname json_class_create
#' @export
#' 
as_json_class.default <- function(x, force = FALSE, ...) {
  if (force)
    x
  else
    error_default(x, "if param force is FALSE, ")
}

#' @rdname json_class_create
#' @export
#' 
rm_json_class <- function(x, recursive = TRUE, restore_type = TRUE) {

  if (is.list(x)) {
    if (is_json_class(x)) {

      if (restore_type) {
        assert_that(!"@type" %in% names(x))
        x <- c(`@type` = get_json_subclass(x), unclass(x))
      } else
        x <- unclass(x)
    }

    if (recursive)
      lapply(x, rm_json_class, recursive, restore_type)
    else
      x
  } else
    x
}

#' @rdname json_class_create
#' @export
#' 
as.list.json_class <- function(x,
                               keep_asis = TRUE,
                               recursive = !keep_asis,
                               restore_type = !keep_asis,
                               ...) {
  if (keep_asis)
    x
  else
    rm_json_class(x, recursive, restore_type)
}

#' Validate JSON class objects
#'
#' The functions `is_json_class()` and `has_json_subclass()` test whether an
#' object is a JSON class object. The former tests whether an object is a
#' proper `json_class` object, meaning that:
#'   * it is a list
#'   * it inherits `json_class`
#'   * the last class attribute is `json_class`
#'   * apart from `json_class` there exists at least one more class attribute
#' The latter function tests whether an object has a specific JSON class
#' attached. In order to recursively test a `json_class` object for being
#' properly formed, the function `check_json_class()` can be used. In order to
#' retrieve the sub-class to a `json_class` object, `get_json_subclass()` can
#' be used.
#' 
#' @param x Object to process.
#' @param class JSON sub-class name.
#' @param recursive Recursively apply the function.
#' 
#' @rdname json_class_validate
#'  
#' @examples
#' lst <- list(`@type` = "foo", "a", "b")
#' cls <- as_json_class(lst)
#' 
#' print(cls)
#' is_json_class(cls)
#' get_json_subclass(cls)
#' 
#' identical(rm_json_class(cls), lst)
#' 
#' @export
#' 
is_json_class <- function(x) {

  isTRUE(is.list(x) &&
         inherits(x, "json_class") &&
         length(class(x)) > 1L &&
         utils::tail(class(x), 1) == "json_class")
}

#' @rdname json_class_validate
#' @export
#' 
is.json_class <- is_json_class

#' @rdname json_class_validate
#' @export
#' 
check_json_class <- function(x, recursive = TRUE) {

  if (inherits(x, "json_class"))
    res <- is_json_class(x)
  else
    res <- TRUE

  if (length(x) > 1 && res && recursive)
    all(sapply(x, check_json_class, recursive))
  else
    res
}

#' @rdname json_class_validate
#' @export
#' 
has_json_subclass <- function(x, class) {

  if (!is_json_class(x))
    return(FALSE)

  assert_that(is.character(class))
  isTRUE(all(class == get_json_subclass(x)))
}

#' @rdname json_class_validate
#' @export
#' 
get_json_subclass <- function(x) {

  assert_that(is_json_class(x))

  setdiff(class(x), "json_class")
}

#' @export
`[.json_class` <- function(x, i, ...) {
  r <- NextMethod("[")
  class(r) <- class(x)
  r
}

#' @export
c.json_class <- function(x, ...) c(as_json_vec(x), json_vec(...))
