
#' @import assertthat
NULL

#' Alias for `as.list()`
#'
#' For consistency with other functions provided by this package, where for
#' example both `is_json_vec()` and `is.json_vec()` are available, `as_list()`
#' is provided as an alias for `as.list()`. Consequently, functions such as
#' `as.list.json_vec()` are also available as `as_list.json_vec()`.
#' 
#' @export
#' 
as_list <- as.list
