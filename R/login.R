
#' Create and destroy a login token
#'
#' Login tokens for openBis API calls can be created using `login_openbis()`.
#' If the `auto_disconnect` option is enabled, the user is automatically
#' logged out using `logout_openbis()` upon garbage collection of the token.
#' The validity of a token can be checked using `is_token_valid()` and a login
#' token can be manually destroyed by calling `logout_openbis()` on the token.
#' 
#' @param user,pwd Login credentials for an openBis instance.
#' @param host_url Host url.
#' @param auto_disconnect Logical switch for automatically closing the
#' connection upon garbage collection of the token.
#' @param ... Further arguments are forwarded to [make_request()].
#' 
#' @rdname login
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "tryToAuthenticateForAllServices")}
#' 
#' @examples
#' \donttest{
#'   # create a login token
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#'   # check token
#'   is_token_valid(tok)
#' 
#'   # destroy token
#'   logout_openbis(tok)
#'   # token is no longer valid
#'   is_token_valid(tok)
#' }
#' 
#' @export
#' 
login_openbis <- function(user,
                          pwd,
                          host_url = "https://infectx.biozentrum.unibas.ch",
                          auto_disconnect = TRUE,
                          ...) {

  disco <- function(tok, ...) {

    reg.finalizer(
      environment(),
      function(...) {
        args <- c(tok, dots)
        attr(args[[1L]], "host_url") <- host_url
        if (do.call(is_token_valid, args)) {
          message("please call logout_openbis() when no longer using a token.")
          do.call(logout_openbis, args)
        }
        invisible(NULL)
      },
      onexit = TRUE
    )

    environment()
  }

  dots <- list(...)

  assert_that(is.string(user), is.string(pwd))

  token <- unlist(make_request(api_url("gis", host_url, ...),
                               "tryToAuthenticateForAllServices",
                               list(user, pwd),
                               finally = function(x) x$result,
                               ...))

  assert_that(is.character(token), length(token) == 1L,
              msg = "Login failed: credentials not valid.")

  attr(token, "host_url") <- host_url

  if (auto_disconnect)  {
    attr(token, "finaliser") <- disco(token, ...)
  }

  token
}

#' @param token Login token as created by `login_openbis()`.
#' @rdname login
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "logout")}
#' @export
#' 
logout_openbis <- function(token, ...)
  invisible(unlist(make_request(api_url("gis", attr(token, "host_url"), ...),
                                "logout",
                                list(token),
                                finally = function(x) x$result,
                                ...)))

#' @rdname login
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "isSessionActive")}
#' @export
#' 
is_token_valid <- function(token, ...)
  unlist(make_request(api_url("gis", attr(token, "host_url"), ...),
                      "isSessionActive",
                      list(token),
                      ...))
