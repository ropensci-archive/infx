
#' JSON class utilities
#'
#' The generic function `has_fields()` tests whether a single `json_class`
#' object contains all of the specified fields or whether each `json_class`
#' object contained in a `json_vec` object passes this test. If dispatch
#' occurs on an object that is neither of class `json_class`, nor of class
#' `json_vec`, `has_fields()` returns `FALSE`. In order to test whether a
#' `json_class` or a `json_vec`  object is of a certain sub-class (can also be
#' a vector of sub-classes), the generic function `has_subclass()` can be
#' used. Dispatch on any other type objects will return `FALSE`. The sub-class
#' of a `json_class` or a `json_vec`  object can be determined, using
#' `get_subclass`. This will also work if dispatched on a `list` of objects if
#' that list object passes [has_common_subclass()].
#' 
#' The function `remove_null()` recursively removes all NULL fields from a
#' nested list structure while preserving `json_class` and `json_vec` class
#' attributes. This can be useful when fetching an object form openBIS and
#' subsequently using this object for a further query: whenever the object
#' returned by the first API call contains NULL fields, it is safer to remove
#' all of them, as in some cases this might cause an error in the following
#' API requests.
#' 
#' @param x Object to test.
#' @param fields Character vector of nonzero length, holding the field names
#' for which to check.
#' @param ... Generic compatibility.
#' 
#' @rdname json_utils
#'  
#' @examples
#' obj_1 <- json_class(a = 1, b = 2, class = "foo")
#' obj_2 <- json_class(a = 3, b = 4, class = "foo")
#' obj_3 <- json_class(a = 3, c = 4, class = "foo")
#' 
#' has_fields(obj_1, "a")
#' has_fields(obj_1, c("a", "b"))
#' 
#' has_fields(c(obj_1, obj_2), "a")
#' has_fields(c(obj_1, obj_3), "a")
#' has_fields(c(obj_1, obj_3), c("a", "b"))
#' 
#' @export
#' 
has_fields <- function(x, fields, ...) {

  assert_that(is.character(fields),
              length(fields) >= 1L)

  UseMethod("has_fields", x)
}

#' @rdname json_utils
#' @export
#' 
has_fields.default <- function(x, ...) FALSE

#' @param class Character vector of nonzero length, holding the class names
#' to test for.
#' 
#' @rdname json_utils
#' @export
#' 
has_subclass <- function(x, class, ...) {

  assert_that(is.character(class),
              length(class) >= 1L)

  UseMethod("has_subclass", x)
}

#' @rdname json_utils
#' @export
#' 
has_subclass.default <- function(x, ...) FALSE

#' @rdname json_utils
#' @export
#' 
get_subclass <- function(x)
  UseMethod("get_subclass")

#' @rdname json_utils
#' @export
#' 
get_subclass.list <- function(x, ...) {
  assert_that(has_common_subclass(x))
  unlist(unique(lapply(x, get_subclass)))
}

#' @rdname json_utils
#' 
#' @examples
#' tmp <- json_class(a = json_class(b = "c", d = NULL, class = "foo"),
#'                   e = json_class(f = "g", class = "bar"),
#'                   h = NULL,
#'                   class = "foobar")
#' print(tmp, 2)
#' print(remove_null(tmp), 2)
#' 
#' @export
#' 
remove_null <- function(x) {
  if (is.list(x)) {
    tmp <- Filter(Negate(is.null), lapply(x, remove_null))
    if (is_json_class(x))
      new_json_class(tmp, class = get_subclass(x))
    else if (is_json_vec(x))
      new_json_vec(tmp)
    else
      tmp
  } else
    x
}

#' Helper function for printing JSON objects
#' 
#' This function powers the `json_class` and `json_vec` specific methods of the
#' base generic [base::print()]. As it is applied recursively and recursion
#' depth has to be controllable, the function is aware of both the current
#' recursion depth (via `cur_depth`) and the maximally allowed recursion depth
#' (via `max_depth`). Furthermore the printing style (colored output and UTF
#' box characters for visualizing the tree structure) can be controlled through
#' the `layout` argument. Under some circumstances, this requires a given node
#' to know whether the parent node is a named object or not, which is passed
#' from a parent node to its children through the `unnamed_parent` argument.
#'
#' @param x The JSON object to print.
#' @param unnamed_parent Whether the parent node is named or not (in some
#' cases, a different box character has to be used if this is true).
#' @param cur_depth The current recursion depth.
#' @param max_depth The maximum recursion depth.
#' @param layout Characters for printing the tree structure and styles to be
#' applied to the different entities.
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters.
#' 
#' @keywords internal
#' @rdname json_internal
#' 
print_json_class <- function(x,
                             unnamed_parent = FALSE,
                             cur_depth,
                             max_depth,
                             layout = style()) {

  indent <- function(x, first, rest) {
    if (length(x) == 1)
      paste0(first, x)
    else
      c(paste0(first, x[[1]]), paste0(rest, x[-1L]))
  }

  if (!is.list(x)) {

    if (length(x) > 1L)
      layout$val(paste0("(", paste(x, collapse = ", "), ")"))
    else
      layout$val(paste(x))

  } else {

    if (is.null(names(x)))
      nme <- rep("", length(x))
    else {
      nme <- paste(names(x), "= ")
      nme[names(x) == ""] <- ""
    }

    if (!is_json_class(x) && !any(sapply(x, is.list))) {

      layout$val(paste0("[", paste0(nme, x, collapse = ", "), "]"))

    } else {

      cur_depth <- cur_depth + 1

      if (cur_depth <= max_depth) {

        if (any(sapply(x, is.null) | sapply(x, length) == 0L))
          x[sapply(x, is.null) | sapply(x, length) == 0L] <- ""

        rest <- Map(indent,
                    mapply(print_json_class, x, nme == "",
                           MoreArgs = list(cur_depth = cur_depth,
                                           max_depth = max_depth,
                                           layout = layout), SIMPLIFY = FALSE),
                    layout$key(nme),
                    sapply(nchar(nme),
                           function(n) paste(rep(" ", n), collapse = "")))
      } else
        rest <- "..."

      if (is_json_class(x)) {
        c(indent(layout$obj(class(x)[1]), paste0(layout$n, layout$h),
                            paste0(layout$v,  " ")),
          unlist(lapply(rest[-length(rest)], indent,
                        paste0(layout$j, layout$h), paste0(layout$v,  " "))),
          indent(rest[[length(rest)]], paste0(layout$l, layout$h), "  ")
        )
      } else if (cur_depth <= max_depth) {
        if (length(rest) == 1L) {
          indent(rest[[1]], paste0(if (!unnamed_parent) layout$h, layout$h),
                 "  ")
        } else {
          c(indent(rest[[1]],
                   paste0(if (unnamed_parent) layout$c else layout$t,
                          layout$h), paste0(layout$v,  " ")),
            unlist(lapply(rest[-c(1, length(rest))], indent,
                          paste0(layout$j, layout$h), paste0(layout$v,  " "))),
            indent(rest[[length(rest)]], paste0(layout$l, layout$h), "  ")
          )
        }
      } else {
        "..."
      }
    }
  }
}

#' Style function for printing JSON objects
#' 
#' In order to enable fancy printing (colored output and UTF box characters
#' for visualizing the tree structure), this function provides the required
#' styling information. Fancy printing can be disabled by setting the `fancy`
#' argument to `FALSE`, which yields ASCII characters for the tree structure
#' and disables color.
#'
#' @keywords internal
#' @rdname json_internal
#' 
style <- function(fancy = TRUE) {

  if (fancy && l10n_info()$`UTF-8`) {

    box <- list(h = "\u2500", # ─ horizontal
                t = "\u250C", # ┌ top
                v = "\u2502", # │ vertical
                l = "\u2514", # └ leaf
                c = "\u252C", # ┬ junction
                j = "\u251C", # ├ junction
                n = "\u2588") # █ node

  } else {

    box <- list(h = "-",
                t = "//",
                v = "|",
                l = "\\",
                c = "+",
                j = "+",
                n = "X")
  }

  if (fancy && crayon::has_color()) {

    # nocov start
    obj_col <- crayon::magenta
    key_col <- crayon::silver
    val_col <- crayon::yellow

    box$n <- obj_col(box$n)

    c(box,
      obj = crayon::combine_styles(crayon::underline, obj_col),
      key = crayon::combine_styles(crayon::italic, key_col),
      val = val_col)
    # nocov end

  } else {

    c(box,
      obj = identity,
      key = identity,
      val = identity)
  }
}
