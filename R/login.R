
#' Create and destroy a login token
#'
#' Login tokens for openBis API calls can be created using `login_openbis()`.
#' If the `auto_disconnect` option is enabled, the user is automatically
#' logged out using `logout_openbis()` upon garbage collection of the token.
#' The validity of a token can be checked using `is_token_valid()` and a login
#' token can be manually destroyed by calling `logout_openbis()` on the token.
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
#' \dontrun{
#'   # create a login token
#'   tok <- login_openbis("rdgr2014", "IXPubReview", auto_disconnect = FALSE)
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

  token <- unlist(make_request(api_url("gis"),
                               "tryToAuthenticateForAllServices",
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
  invisible(unlist(make_request(api_url("gis"), "logout", list(token),
                                finally = function(x) x$result)))

#' @rdname login
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "isSessionActive")}
#' @export
#' 
is_token_valid <- function(token)
  unlist(make_request(api_url("gis"), "isSessionActive", list(token)))
