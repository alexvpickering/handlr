
#' Handles incomming rApache requests
#'
#' @param GET rApache variable: list of parameters
#' @param POST rApache variable: list of parameters
#' @param COOKIES rApache variable: list of cookies
#' @param FILES rApache variable:
#' @param SERVER rApache variable: list
#' @param allowed_packages character vector of allowed packages
#' @param timeout maximum time in seconds to allow for call to return
#' @param rlimits named vector/list with RLIMIT values, for example: c(cpu = 60, fsize = 1e6)
#'
#' @return
#' @export
#'
#' @examples
handle <- function(GET, POST, COOKIES, FILES, SERVER,
                   allowed_packages, timeout = 0, rlimits = NULL) {

  params   <- get_params(GET, POST, SERVER)
  endpoint <- get_endpoint(SERVER)

  if (!valid_endpoint(endpoint, allowed_packages)) return()

  # get function to call (same as pkg::name)
  what <- getExportedValue(endpoint[1], endpoint[2])

  # call function in forked process with specified limits
  result <- sys::eval_safe(
    do.call(what, params),
    timeout = timeout,
    rlimits = rlimits
  )

  # return JSON object
  set_status(200L, message)
  as.vector(jsonlite::toJSON(result))

}


#'
#'
#' Helper function to permit testing. Instead of setting status directly,
#' checks if environment is rApache and only calls setStatus if so.
#'
#' @param status
#' @param message
#'
#' @return
#'
#' @examples
set_status <- function(status, message) {
  if (is_rapache()) {
    setStatus(status=status)
    cat(message)
  }
}

#' Check if endpoint is allowed/exists
#'
#' @param endpoint Character vector length two: package and function name
#' @param allowed_packages Character vector of allowed package endpoints
#'
#' @return
#'
#' @examples
valid_endpoint <- function(endpoint, allowed_packages) {

  # check if package is allowed
  if (!endpoint[1] %in% allowed_packages) {

    message <- paste('Package', endpoint[1], 'is not an allowed package.\n')
    set_status(403L, message)
    return(FALSE)
  }

  # check if package exports function
  package_functions <- get_exports(endpoint[1])
  if (!endpoint[2] %in% package_functions) {

    message <- paste0('Function ', endpoint[2], ' is not exported by package ', endpoint[1], '.\n')
    set_status(404L, message)
    return(FALSE)
  }

  return(TRUE)
}

#' Reads the NAMESPACE file for installed package
#'
#' @param package Package name to read NAMESPACE from
#'
#' @return character vector of exports from package
#'
#' @examples
get_exports <- function(package) {

  f <- base::system.file("NAMESPACE", package=package)
  objs <- readLines(f)
  exps <- objs[grepl("export", objs)]
  sub("^export[^\\(]*\\(([^\\)]+)\\)", "\\1", exps)
}



#' Checks if rApache is being used
#'
#' @return
#' @keywords internal
#'
#' @examples
is_rapache <- function() {
  if (exists('setStatus')) return(TRUE)
  return(FALSE)
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

#' Gets the package and function names from a path
#'
#' @param SERVER rApache variable: list
#'
#' @return character vector of length 2 with first string being the package name and the second the function name
#' @keywords internal
#'
#' @examples
get_endpoint <- function(SERVER) {

  endpoint <- strsplit(SERVER$path_info, '/')[[1]][2:3]
  return(endpoint)
}


