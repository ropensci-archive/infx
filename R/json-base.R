
#' Base generics for JSON objects
#' 
#' Available base generic functions for objects that inherit `json_class` are
#' [base::print()], \code{\link[base:[]{base::[()}}, [base::c()] and
#' [base::rep()] and for `json_vec` objects, all of the above in addition
#' to \code{\link[base:[<-]{base::[<-()}} and
#' \code{\link[base:[[<-]{base::[[<-()}} are implemented. For further
#' information on how these class-specific functions differ from their base
#' counterparts, refer to the details section.
#' 
#' Single bracket sub-setting of `json_class` objects preserves
#' class information, such that the resulting object has the same type but only
#' a subset of fields. Double bracket sub-setting of `json_class` objects
#' removes the enclosing type, as would be expected considering the list nature
#' of `json_class` objects. Combining or repeating `json_class` objects yields
#' `json_vec` objects with the same sub-type. Additionally, when combining
#' `json_class` objects using [base::c()], only `json_class` objects with the
#' same subtype as the first argument are allowed as further arguments.
#' 
#' Analogously to sub-setting of `json_class` objects, sub-setting a `json_vec`
#' object with \code{\link[base:[]{base::[()}} returns a `json_vec` object
#' with the same sub type as the one used as input, whereas sub-setting a
#' `json_vec` object with \code{\link[base:[[]{base::[[()}} yields the
#' selected `json_class` object. Replacement operators
#' \code{\link[base:[<-]{base::[<-()}} and
#' \code{\link[base:[[<-]{base::[[<-()}} mainly ensure that the objects
#' being inserted are of the correct sub-type, guaranteeing that all
#' `json_class` members of a given `json_vec` object are of the same sub-type.
#' Combining `json_vec` objects with [base::c()] is possible whenever the
#' object passed as first argument has the same sub-type as the objects passed
#' as further arguments, which additionally are required to be `json_vec`
#' objects as well. Repeating a `json_vec` object using [base::rep()], results
#' in a `json_vec` object of the same sub-type.
#'
#' Printing of both `json_class` and `json_vec` objects is inspired by the
#' `ast` printing function of Hadley's
#' [`lobstr` package](https://git.io/vFMA5) and borrows code from there.
#' Printing style can either be fancy (colors, UTF box characters. etc.) or
#' simple (controlled by the `fancy` flag) and several options are available
#' for setting the max printing width/length, as well as a max recursion depth
#' for nested `json_class` objects.
#' 
#' @param x Object to print/combine/subset, etc.
#' @param depth The maximum recursion depth for printing.
#' @param width Number of columns to maximally print.
#' @param length Number of lines to maximally print.
#' @param fancy Logical switch to enable font styles, colors and UTF box
#' characters for printing.
#' @param ... Generic compatibility.
#' 
#' @rdname json_base
#' 
#' @family json object handling functions
#' 
#' @return Depending on whether a single or a set of multiple objects is
#' represented, the S3 classes [`json_class`] or [`json_vec`] are applied
#' respectively.
#'  
#' @examples
#' obj_c <- json_class(a = json_class(b = "c", class = "foo"),
#'                     d = json_class(e = "f", class = "bar"),
#'                     class = "foobar")
#' obj_c
#' print(obj_c, depth = 2L)
#' print(obj_c, depth = 2L, length = 4L)
#' print(obj_c, depth = 2L, fancy = FALSE)
#' 
#' # sub-setting with single brackets preserves class information
#' obj_c["a"]
#' # whereas double brackets extract the selected element
#' obj_c[["a"]]
#' 
#' # vectors of json_class objects are json_vec objects
#' obj_cc <- rep(obj_c, 2)
#' identical(obj_cc, c(obj_c, obj_c))
#' 
#' print(obj_cc, depth = 2L, length = 8L)
#' 
#' obj_g <- json_class(a = json_class(b = "g", class = "foo"),
#'                     d = json_class(e = "h", class = "bar"),
#'                     class = "foobar")
#' obj_cg <- c(obj_c, obj_g)
#' 
#' # sub-setting json_vec objects with single brackets yields json_vec objects
#' class(obj_cg[1L])
#' # and with double brackets, the selected json_class object is extracted
#' class(obj_cg[[1L]])
#' identical(obj_cg[1L], json_vec(obj_cg[[1L]]))
#' 
#' # json_vec objects can also be combined using c
#' obj_i <- json_class(a = json_class(b = "i", class = "foo"),
#'                     d = json_class(e = "j", class = "bar"),
#'                     class = "foobar")
#' 
#' obj_cgi <- c(obj_cg, json_vec(obj_i))
#' length(obj_cgi)
#' 
#' # and repeated using rep
#' length(rep(obj_cgi, 2))
#' 
#' # additionally replacement operators are available
#' obj_cg[[2L]] <- obj_i
#' obj_cgi[1L:2L] <- obj_cg
#' identical(obj_cgi, c(obj_c, obj_i, obj_i))
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

#' @param i Index for sub-setting. See \code{\link[base:[]{base::[()}} and
#' \code{\link[base:[[]{base::[[()}}.
#' 
#' @rdname json_base
#' @export
#' 
`[.json_class` <- function(x, i, ...) {
  r <- NextMethod("[")
  class(r) <- class(x)
  r
}

#' @rdname json_base
#' @export
#' 
c.json_class <- function(x, ...) c(as_json_vec(x), json_vec(...))

#' @rdname json_base
#' @export
#' 
rep.json_class <- function(x, ...) {
  x <- as_json_vec(x)
  as_json_vec(NextMethod())
}

#' @rdname json_base
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
    out <- c(paste0(form$h, form$h, out[[1L]][1L]),
             paste(" ", out[[1L]][-1]))
  } else {
    out <- c(paste0(form$t, form$h, out[[1L]][1L]),
             paste(form$v, out[[1L]][-1]),
             unlist(lapply(out[-c(1, length(out))], function(y) {
               c(paste0(form$j, form$h, y[1L]), paste(form$v, y[-1]))
             })),
             paste0(form$l, form$h, out[[length(out)]][1L][1L]),
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

#' @rdname json_base
#' @export
#' 
`[.json_vec` <- function(x, i, ...) {
  new_json_vec(NextMethod())
}

#' @param value New values for replacement. See
#' \code{\link[base:[<-]{base::[<-()}} and
#' \code{\link[base:[[<-]{base::[[<-()}}.
#' 
#' @rdname json_base
#' @export
#' 
`[<-.json_vec` <- function(x, i, ..., value) {

  assert_that(get_subclass(value) == get_subclass(x))

  if (is_json_class(value))
    value <- list(value)

  NextMethod()
}

#' @rdname json_base
#' @export
#' 
`[[.json_vec` <- function(x, i, ...) {
  res <- NextMethod()
  assert_that(is_json_class(res))
  res
}

#' @rdname json_base
#' @export
#' 
`[[<-.json_vec` <- function(x, i, ..., value) {

  assert_that(get_subclass(value) == get_subclass(x))

  NextMethod()
}

#' @rdname json_base
#' @export
#' 
c.json_vec <- function(x, ...) as_json_vec(NextMethod())

#' @rdname json_base
#' @export
#' 
rep.json_vec <- function(x, ...) as_json_vec(NextMethod())

#' @rdname json_base
#' @export
#' 
as_list <- as.list
