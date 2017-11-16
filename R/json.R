
#' @title Make a JSON-RPC request
#'
#' @description Issues a POST request to a JSON-RPC server. All "@type" fields
#' are converted to/from "json_class" attributes.
#' 
#' @param url Url, the request is sent to.
#' @param method The method name
#' @param params A list structure holding the arguments which, converted to
#' JSON, will be used to call the supplied method. The "@type" entries will be
#' generated from "json_class" attributes.
#' @param version JSON-RPC protocol version to be used.
#' @param id Id of the JSON-RPC request.
#' 
#' @return A (nested) list holding the response from the JSON-RPC server
#' ("@type" entries are converted to "json_class" attributes).
#' 
make_request <- function(url,
                         method,
                         params,
                         version = "2.0",
                         id = "1") {

  req <- list(id = id,
              jsonrpc = version,
              method = method,
              params = rm_json_class(params))

  res <- httr::POST(url, body = req, encode = "json")

  assert_that(res$status_code == 200)

  res$content <- jsonlite::fromJSON(rawToChar(res$content),
                                    simplifyVector = FALSE)

  if (!is.null(res$content$error))
    stop("Error:\n", paste(names(res$content$error), res$content$error,
                           sep = ": ", collapse = "\n"))

  add_json_class(res$content$result)
}

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

#' @title Print a JSON object
#'
#' @description Prints a JSON object stored as a possibly nested list
#' structure. The JSON objects themselves are rendered as nodes and will be
#' traversed recursively up to the specified depth. Furthermore, the maximum
#' width (number of columns printed) and length (number of lines printed) may
#' be specified.
#' 
#' @rdname json_class
#' 
#' @param depth The maximum recursion depth.
#' @param width Number of columns to maximally print.
#' @param length Number of lines to maximally print.
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters.
#' @param ... Generic compatibility.
#' 
#' @return The input object (invisibly).
#' 
#' @export
#' 
print.json_class <- function(x,
                             depth = 1L,
                             width = getOption("width"),
                             length = 100L,
                             fancy = TRUE,
                             ...) {

  out <- print_json_class(x, depth = 0L, max_depth = depth,
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

#' @title Helper function for printing JSON objects
#'
#' @description Inspired by the ast printing function of Hadley's [lobstr
#' package](https://github.com/hadley/lobstr) and also heavily borrowing code
#' from [there](https://git.io/vFMA5), this enables the recursive printing of
#' JSON objects.
#' 
#' @param x The JSON object to print.
#' @param depth The current recursion depth.
#' @param max_depth The maximum recursion depth.
#' @param layout Characters for printing the tree structure and styles to be
#' applied to the different entities.
#' 
#' @return A string that can be used for printing.
#' 
print_json_class <- function(x, depth, max_depth, layout = style()) {

  indent <- function(x, first, rest) {
    if (length(x) == 1)
      paste0(first, x)
    else
      c(paste0(first, x[[1]]), paste0(rest, x[-1L]))
  }

  if (!is_json_class(x))

    layout$val(paste(x))

  else {

    depth <- depth + 1

    obj <- indent(layout$obj(attr(x, "json_class")),
                  paste0(layout$n, layout$h),
                  paste0(layout$v,  " "))

    if (depth <= max_depth) {

      if (any(sapply(x, is.null))) x[sapply(x, is.null)] <- ""

      rest <- Map(indent,
                  lapply(x, print_json_class, depth, max_depth,
                         layout = layout),
                  layout$key(paste0(names(x), " = ")),
                  strrep(" ", nchar(names(x)) + 3))
    } else
      rest <- "..."

    c(obj,
      unlist(lapply(rest[-length(rest)], indent, paste0(layout$j, layout$h),
                    paste0(layout$v,  " "))),
      indent(rest[[length(rest)]], paste0(layout$l, layout$h), "  ")
    )
  }
}

#' @title Styles for printing JSON objects
#'
#' @description Characters for printing the tree structure and styles to be
#' applied to the different entities in [print_json_class].
#' 
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters.
#' 
#' @return A list holding the tree characters and the entity styling.
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

    obj_col <- crayon::magenta
    key_col <- crayon::silver
    val_col <- crayon::yellow

    box$n <- obj_col(box$n)

    c(box,
      obj = crayon::combine_styles(crayon::underline, obj_col),
      key = crayon::combine_styles(crayon::italic, key_col),
      val = val_col)

  } else {

    c(box,
      obj = identity,
      key = identity,
      val = identity)
  }
}