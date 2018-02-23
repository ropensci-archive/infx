
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

#' @export
`[.json_class` <- function(x, i, ...) {
  r <- NextMethod("[")
  class(r) <- class(x)
  r
}

#' @export
c.json_class <- function(x, ...) c(as_json_vec(x), json_vec(...))

#' @export
rep.json_class <- function(x, ...) {
  x <- as_json_vec(x)
  as_json_vec(NextMethod())
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

#' @export
`[.json_vec` <- function(x, i, ...) {
  new_json_vec(NextMethod())
}

#' @export
`[<-.json_vec` <- function(x, i, ..., value) {

  sub_class <- get_subclass(x)

  assert_that(get_subclass(value) == sub_class)

  if (is_json_class(value))
    value <- list(value)

  NextMethod()
}

#' @export
`[[.json_vec` <- function(x, i, ...) {
  res <- NextMethod()
  assert_that(is_json_class(res))
  res
}

#' @export
`[[<-.json_vec` <- function(x, i, ..., value) {

  assert_that(get_subclass(value) == get_subclass(x))

  NextMethod()
}

#' @export
c.json_vec <- function(x, ...) as_json_vec(NextMethod())

#' @export
rep.json_vec <- function(x, ...) as_json_vec(NextMethod())
