
#' Handles incomming rApache requests
#'
#' @param GET rApache variable: list of parameters
#' @param POST rApache variable: list of parameters
#' @param COOKIES rApache variable: list of cookies
#' @param FILES rApache variable:
#' @param SERVER rApache variable: list
#' @param packages character vector of allowed packages
#'
#' @return
#' @export
#'
#' @examples
handle <- function(GET, POST, COOKIES, FILES, SERVER, packages) {
  check_rapache()

  params <- get_params(GET, POST, SERVER)
  fun <- get_fun(SERVER)

  test_package(fun[1], packages)


  setStatus(status=200L)
  cat(paste('Function', fun, 'was found. Yay!'))

}

test_package <- function(package, packages) {
  if (!package %in% packages) {
    setStatus(status=404L)
    cat(paste('Package', package, 'is not an allowed package.'))
    stop()
  }
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
#' @return character vector of length 2 with first being the package and second the function
#' @keywords internal
#'
#' @examples
get_fun <- function(SERVER) {

  fun <- strsplit(SERVER$path_info, '/')[[1]][2:3]


  return(fun)
}


