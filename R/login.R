
#' Generate/check/destroy a login token
#'
#' Create a login token for openBis API calls. If the `auto_disconnect` option
#' is chosen, the user is logged out automatically upon garbage collection of
#' the token. The validity of a token can be checked using `is_token_valid()`
#' and a login token can be destroyed using `logout_openbis()`.
#' 
#' @param user,pwd Login credentials for an openBis instance.
#' @param auto_disconnect Logical switch for automatically closing the
#' connection upon garbage collection of the token.
#' @param token Login token as created by `login_openbis()`.
#' 
#' @rdname login
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "tryToAuthenticateForAllServices")}
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis("rdgr2014", "IXPubReview", auto_disconnect = FALSE)
#'   is_token_valid(tok)
#'   logout_openbis(tok)
#'   is_token_valid(tok)
#' }
#' 
#' @export
#' 
login_openbis <- function(user,
                          pwd,
                          auto_disconnect = TRUE) {

  disco <- function(tok) {

    reg.finalizer(
      environment(),
      function(...) {
        message("please call \"logout_openbis\" when no longer using a token.")
        logout_openbis(tok)
      },
      onexit = TRUE
    )

    environment()
  }

  token <- unlist(request_openbis("tryToAuthenticateForAllServices",
                                  list(user, pwd)))

  assert_that(is.character(token), length(token) == 1L, msg = "Login failed.")

  if (auto_disconnect)  {
    attr(token, "finaliser") <- disco(token)
  }

  token
}

#' @rdname login
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "logout")}
#' @export
#' 
logout_openbis <- function(token)
  invisible(unlist(request_openbis("logout", list(token))))

#' @rdname login
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "isSessionActive")}
#' @export
#' 
is_token_valid <- function(token)
  unlist(request_openbis("isSessionActive", list(token)))
