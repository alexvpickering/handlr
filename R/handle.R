
#' Handles incomming rApache requests
#'
#' @param GET rApache variable: list of parameters
#' @param POST rApache variable: list of parameters
#' @param COOKIES rApache variable: list of cookies
#' @param FILES rApache variable:
#' @param SERVER rApache variable: list
#'
#' @return
#' @export
#'
#' @examples
handle <- function(GET, POST, COOKIES, FILES, SERVER) {
  check_rapache()

  params <- get_params(GET, POST, SERVER)
  fun <- get_fun(SERVER)

  if (!is.function(fun)) {
    setStatus(status=404L)
    cat(paste('Function', fun, 'was not found.'))
    return()
  }

  setStatus(status=200L)
  cat(paste('Function', fun, 'was found. Yay!'))

}

#' Throws error if rApache not being used
#'
#' @return
#' @keywords internal
#'
#' @examples
check_rapache <- function() {
  stopifnot(is.function(setStatus))
}


#' Get parameters from API call
#'
#' @param GET rApache variable: list of parameters
#' @param POST rApache variable: list of parameters
#' @param SERVER rApache variable: list
#'
#' @return params named list
#' @keywords internal
#'
#' @examples
get_params <- function(GET, POST, SERVER) {

  params <- switch(SERVER$method,
                   'GET' = GET,
                   'POST' = POST)
  return(params)
}

#' Gets the first relative endpoint (function name) from a path
#'
#' @param SERVER rApache variable: list
#'
#' @return endoint character
#' @keywords internal
#'
#' @examples
get_fun <- function(SERVER) {

  fun <- strsplit(SERVER$path_info, '/')[[1]][2]
  return(fun)
}


