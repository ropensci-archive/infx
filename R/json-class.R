
#' JSON class objects
#'
#' To communicate object type information via JSON to the Jackson-powered
#' openBis interface, the `@type` field is used. Furthermore, the `@id` field
#' is used by the JSON-RPC specification to map requests in async scenarios.
#' Data received from openBis is  recursively stripped of both `@type` and
#' `@id` fields and the type information is saved as class attribute. Such
#' objects also have the class `json_class` added.
#' 
#' The action of [as_json_class()] is reversed by [as_json_list()], which
#' removes both the `json_class` class attribute and the JSON class attribute
#' itself, which is subsequently written to a `@type` filed. This action is
#' recursively applied to lists.
#' 
#' The functions [is_json_class()] and [has_json_subclass()] test whether an
#' object is a JSON class object. The former tests whether an object is a
#' proper `json_class` object, meaning that:
#'   * it inherits `json_class`
#'   * the last class attribute is `json_class`
#'   * apart from `json_class` there exists at least one more class attribute
#' The latter function tests whether an object has a specific JSON class
#' attached.
#' 
#' JSON class objects have custom sub-setting and printing functions available.
#' Sub-setting of JSON objects that preserve class and `json_class`
#' attributes. This is useful when objects are created from openBIS results
#' which are subsequently used in further queries, but the constructors they
#' are passed to require only a subset of the fetched fields. Printing is
#' inspired by (and heavily borrows code from) the ast printing function of
#' Hadley's [lobstr package](https://github.com/hadley/lobstr).
#' 
#' @param x Object to process.
#' @param class JSON class name used to test for.
#' @param i Sub-setting information.
#' @param depth The maximum recursion depth for printing.
#' @param width Number of columns to maximally print.
#' @param length Number of lines to maximally print.
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters for printing.
#' @param ... Generic compatibility.
#' 
#' @rdname json_class
#'  
#' @export
#' 
as_json_class <- function(x) {

  if (is.list(x) && "@type" %in% names(x)) {

    assert_that(sum("@type" == names(x)) == 1L,
                is.character(x[["@type"]]),
                length(x[["@type"]]) == 1L)

    x <- structure(x[!names(x) %in% c("@type", "@id")],
                   class = c(x[["@type"]], "json_class"))
  }

  sublist <- sapply(x, is.list)

  if (any(sublist))
    x[sublist] <- lapply(x[sublist], as_json_class)

  x
}

#' @rdname json_class
#' @export
#' 
as_json_list <- function(x) {

  if (is_json_class(x))
    x <- c(`@type` = class(x)[1], unclass(x))

  sublist <- sapply(x, is.list)

  if (any(sublist))
    x[sublist] <- lapply(x[sublist], as_json_list)

  x
}

#' @rdname json_class
#' @export
#' 
is_json_class <- function(x) {

  if (inherits(x, "json_class"))
    isTRUE(length(class(x)) > 1L && utils::tail(class(x), 1) == "json_class")
  else
    FALSE
}

#' @rdname json_class
#' @export
#' 
has_json_subclass <- function(x, class) {

  if (!is_json_class(x))
    FALSE
  else {
    assert_that(is.character(class))
    setequal(class, get_json_subclass(x))
  }
}

#' @rdname json_class
#' @export
#' 
get_json_subclass <- function(x) {

  assert_that(is_json_class(x))

  setdiff(class(x), "json_class")
}

#' @rdname json_class
#' @export
#' 
`[.json_class` <- function(x, i, ...) {
  r <- NextMethod("[")
  class(r) <- class(x)
  r
}

#' @rdname json_class
#' @export
#' 
print.json_class <- function(x,
                             depth = 1L,
                             width = getOption("width"),
                             length = 100L,
                             fancy = TRUE,
                             ...) {

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

#' Helper functions for printing JSON objects
#'
#' Inspired by the ast printing function of Hadley's `lobstr` package heavily
#' borrowing code from [there](https://git.io/vFMA5), this enables the
#' recursive printing of JSON objects. Printing style can either be with fancy
#' (colors, UTF box characters. etc.) or simple.
#' 
#' @param x The JSON object to print.
#' @param cur_depth The current recursion depth.
#' @param max_depth The maximum recursion depth.
#' @param layout Characters for printing the tree structure and styles to be
#' applied to the different entities.
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters.
#' 
#' @rdname json_print
#' 
#' @keywords internal
#' 
print_json_class <- function(x, cur_depth, max_depth, layout = style()) {

  indent <- function(x, first, rest) {
    if (length(x) == 1)
      paste0(first, x)
    else
      c(paste0(first, x[[1]]), paste0(rest, x[-1L]))
  }

  if (!is_json_class(x)) {

    if (is.list(x))
      layout$val(paste0("[", paste(x, collapse = ", "), "]"))
    else
      layout$val(paste(x))

  } else {

    cur_depth <- cur_depth + 1

    obj <- indent(layout$obj(class(x)[1]),
                  paste0(layout$n, layout$h),
                  paste0(layout$v,  " "))

    if (cur_depth <= max_depth) {

      if (any(sapply(x, is.null) | sapply(x, length) == 0L))
        x[sapply(x, is.null) | sapply(x, length) == 0L] <- ""

      rest <- Map(indent,
                  lapply(x, print_json_class, cur_depth, max_depth,
                         layout = layout),
                  layout$key(if (is.null(names(x))) ""
                             else paste0(names(x), " = ")),
                  strrep(" ", if (is.null(names(x))) 0L
                              else nchar(names(x)) + 3))
    } else
      rest <- "..."

    c(obj,
      unlist(lapply(rest[-length(rest)], indent, paste0(layout$j, layout$h),
                    paste0(layout$v,  " "))),
      indent(rest[[length(rest)]], paste0(layout$l, layout$h), "  ")
    )
  }
}

#' @rdname json_print
#' @keywords internal
#' 
style <- function(fancy = TRUE) {

  if (fancy && l10n_info()$`UTF-8`) {

    box <- list(h = "\u2500", # ─ horizontal
                v = "\u2502", # │ vertical
                l = "\u2514", # └ leaf
                j = "\u251C", # ├ junction
                n = "\u2588") # █ node

  } else {

    box <- list(h = "-",
                v = "|",
                l = "\\",
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