
#' Check JSON objects
#'
#' Test whether a single `json_class` object contains all of the specified
#' fields or whether each `json_class` object contained in a `json_vec` object
#' passes this test.
#' 
#' @param x Object to test.
#' @param fields Character vector of nonzero length, holding the field names
#' for which to check.
#' @param ... Generic compatibility.
#' 
#' @rdname json_check
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

#' @rdname json_check
#' @export
#' 
has_fields.json_class <- function(x, fields, ...)
  all(fields %in% names(x))

#' @rdname json_check
#' @export
#' 
has_fields.json_vec <- function(x, fields, ...)
  all(sapply(x, has_fields, fields))

#' Remove NULL entries
#'
#' Recursively remove all NULL fields from a nested list structure while
#' preserving `json_class` and `json_vec` class attributes.
#' 
#' @param x Object to process.
#' 
#' @rdname remove_null
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

#' Print JSON objects
#'
#' Inspired by the ast printing function of Hadley's `lobstr` package and
#' borrowing code from [there](https://git.io/vFMA5), this enables the
#' recursive printing of `json_class` and `json_vec` objects. Printing style
#' can either be with fancy (colors, UTF box characters. etc.) or simple and
#' several options are available for setting the max printing with/length, as
#' well as a max recursion depth for nested `json_class` objects.
#' 
#' @param x Object to print.
#' @param depth The maximum recursion depth for printing.
#' @param width Number of columns to maximally print.
#' @param length Number of lines to maximally print.
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters for printing.
#' @param ... Generic compatibility.
#' 
#' @rdname json_print
#'  
#' @examples
#' tmp <- json_class(a = json_class(b = "c", class = "foo"),
#'                   d = json_class(e = "f", class = "bar"),
#'                   class = "foobar")
#' tmp
#' print(tmp, depth = 2L)
#' print(tmp, depth = 2L, length = 4L)
#' 
#' @export
#' 
print.json_class <- function(x,
                             depth = 1L,
                             width = getOption("width"),
                             length = 100L,
                             fancy = TRUE,
                             ...) {

  if (!check_json_class(x))
    warning("printing a json_class object that is not properly formed.")

  out <- print_json_class(x, cur_depth = 0L, max_depth = depth,
                          layout = style(fancy))

  too_wide <- crayon::col_nchar(out) > width
  out[too_wide] <- paste0(crayon::col_substr(out[too_wide], 1L,
                                             width - 4L), "...")

  if (length(out) > length) {
    out[length] <- "..."
    out <- out[seq_len(length)]
  }

  cat(paste(out, "\n", collapse = ""), sep = "")
  invisible(x)
}

#' @rdname json_print
#' @export
#' 
print.json_vec <- function(x,
                           depth = 1L,
                           width = getOption("width"),
                           length = 100L,
                           fancy = TRUE,
                           ...) {

  form <- style(fancy)

  out <- lapply(x, print_json_class, cur_depth = 0L, max_depth = depth,
                layout = form)

  if (length(out) == 1L) {
    out <- c(paste0(form$h, form$h, out[[1]][1]),
             paste(" ", out[[1]][-1]))
  } else {
    out <- c(paste0(form$t, form$h, out[[1]][1]),
             paste(form$v, out[[1]][-1]),
             unlist(lapply(out[-c(1, length(out))], function(y) {
               c(paste0(form$j, form$h, y[1]), paste(form$v, y[-1]))
             })),
             paste0(form$l, form$h, out[[length(out)]][1][1]),
             paste(" ", out[[length(out)]][-1]))
  }

  too_wide <- crayon::col_nchar(out) > width
  out[too_wide] <- paste0(crayon::col_substr(out[too_wide], 1L,
                                             width - 3L), "...")

  if (length(out) > length) {
    out[length] <- "..."
    out <- out[seq_len(length)]
  }

  cat(paste(out, "\n", collapse = ""), sep = "")
  invisible(x)
}

# Helper functions for printing JSON objects
#
# @param x The JSON object to print.
# @param unnamed_parent Whether the parent node is named or not (in some
# cases, a different box character has to be used if this is true).
# @param cur_depth The current recursion depth.
# @param max_depth The maximum recursion depth.
# @param layout Characters for printing the tree structure and styles to be
# applied to the different entities.
# @param fancy Logical switch to enable font styles, colors and UTF box
# characters.
# 
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
                    paste(rep(" ", nchar(nme)), collapse = ""))
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
