
#' @title Load yaml configuration file
#' 
#' @description Load a yaml configuration file or a subsection thereof.
#' 
#' @param file_name The file name of the config file. If no value is supplied,
#' the current wd is searched for files ending in '.yaml' or '.yml', followed
#' by the extdata dir in the package installation.
#' @param section A list node to be returned instead of the whole config list.
#' If multiple nodes match, the last match (the list is traversed recursively)
#' will be returned
#' 
#' @return The configuration as a list.
#' 
#' @export
#'
load_config <- function(file_name = NULL,
                        section = NULL) {

  find_yml <- function(dir) {
    list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)[1]
  }

  find_section <- function(lst, sec) {

    recurse <- function(l, s) {
      if (!is.list(l)) NULL
      else if (s %in% names(l)) res <<- l[[s]]
      else lapply(l, recurse, s)
    }

    res <- list()
    recurse(lst, sec)

    stopifnot(length(res) > 0)
    res
  }

  if (!is.null(file_name))
    stopifnot(is.character(file_name), length(file_name) == 1L)
  if (!is.null(section))
    stopifnot(is.character(section), length(section) == 1L)

  if (is.null(file_name)) {

    file_name <- find_yml(getwd())

    if (is.na(file_name)) {
      file_name <- find_yml(system.file("extdata",
                            package = methods::getPackageName()))
    }

    stopifnot(!is.na(file_name))

  } else stopifnot(file.exists(file_name))

  cfg <- yaml::yaml.load_file(file_name)

  if (!is.null(section)) {
    find_section(cfg, section)
  } else
    cfg
}
