
check_skip <- function() {

  if (identical(Sys.getenv("TRAVIS"), "true")) {
    if (identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release"))
      return(invisible(TRUE))
    else
      skip("On non-release Travis")
  }

  if (identical(Sys.getenv("NOT_CRAN"), "true"))
    return(invisible(TRUE))
  else
    skip("On CRAN")
}