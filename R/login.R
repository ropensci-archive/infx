
#' @title Generate a login token
#'
#' @description Create a login token for openBis API calls. Upon garbage
#' collection of the token, the user is logged out.
#' 
#' @param user,pwd Login credentials for an openBis instance.
#' @param auto_disconnect Logical switch for automatically closing the
#' connection upon garbage collection of the token.
#' 
#' @return The login token to be used for further API interactions.
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

  token <- unlist(query_openbis("tryToAuthenticateForAllServices",
                                list(user, pwd)))

  assert_that(is.character(token), length(token) == 1L, msg = "Login failed.")

  if (auto_disconnect)  {
    attr(token, "finaliser") <- disco(token)
  }

  token
}

#' @title Logout from openBis
#'
#' @description Using a token as created by [login_openbis], the corresponding
#' session is closed and the token is rendered invalid.
#' 
#' @param token Login token as created by [login_openbis].
#' 
#' @return NULL (invisibly)
#' 
#' @export
#' 
logout_openbis <- function(token)
  invisible(unlist(query_openbis("logout", list(token))))

#' @title Check validity of token
#'
#' @description A token as created by [login_openbis] is tested for its
#' validity.
#' 
#' @inheritParams logout_openbis
#' 
#' @return Scalar logical.
#' 
#' @export
#' 
is_token_valid <- function(token)
  unlist(query_openbis("isSessionActive", list(token)))
