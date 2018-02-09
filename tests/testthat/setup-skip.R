
check_skip <- function() {

  if (identical(Sys.getenv("TRAVIS"), "true") &&
      !identical(Sys.getenv("TRAVIS_R_VERSION_STRING"), "release"))
    skip("On non-release Travis")

  if (identical(Sys.getenv("APPVEYOR"), "True"))
    skip("On Appveyor")

  if (!identical(Sys.getenv("NOT_CRAN"), "true"))
    skip("On CRAN")

  return(invisible(TRUE))
}