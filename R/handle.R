
#' Handles incomming rApache requests
#'
#' @param SERVER rApache variable.
#' @param packages Vector of packages that api allows.
#' @param open Endpoints that are open to non-authenticated users.
#'    Specify as list of length 2 character vectors
#'    for example: list(c('package_name', 'package_function'))
#' @param key string or raw vector needed to decode JSON web token.
#' @param timeout Time limit in seconds for function to execute.
#' @param rlimits named vector/list with rlimit values,
#'    for example: c(cpu = 60, fsize = 1e6).

#' @return
#' @export
#'
#' @examples
handle <- function(SERVER,
                   packages,
                   open = 'all',
                   secret = NULL,
                   timeout = 0,
                   rlimits = NULL) {

  # lowercase headers
  req_headers <- SERVER$headers_in
  names(req_headers) <- tolower(names(req_headers))

  endpoint <- get_endpoint(SERVER)

  # check if endpoint is allowed/exported
  if(!endpoint_valid(endpoint, packages)) return()

  # get claim if endpoint requires JSON web token
  claim <- if(!endpoint_open(endpoint, open)) {
    validate_jwt(req_headers, secret)
  }


    # POST body to raw
    post <- if (SERVER$method == 'POST') {
      list(
        body  = rapache('receiveBin'),
        ctype = req_headers[['content-type']]
      )
    }


  #
  # # get function to call (same as pkg::name)
  # what <- getExportedValue(endpoint[1], endpoint[2])
  #
  # # call function in forked process with specified limits
  # result <- sys::eval_safe(
  #   do.call(what, params),
  #   timeout = timeout,
  #   rlimits = rlimits
  # )
  #
  # # return JSON object
  # set_status(200L)
  # cat(as.vector(jsonlite::toJSON(result)))
}

#' Validates JSON web token
#'
#' Error occurs if validation fails.
#'
#' @param req_headers List with request headers.
#' @inheritParams handle
#'
#' @return List with claim.
#' @export
#'
#' @examples
validate_jwt <- function(req_headers, secret) {

  if (is.null(secret))
    stop("argument 'secret' must be a string or raw vector")

  jwt <- req_headers[['authorization']]
  jwt <- gsub('^Bearer ', '', jwt)

  jose::jwt_decode_hmac(jwt, secret)
}


endpoint_open <- function(endpoint, open='all') {
  if (open == 'all') return(TRUE)
  any(sapply(open, function(x) identical(x, endpoint)))
}


#' Helper function to permit testing.
#'
#' Calls rApache function only if rapache environment
#'
#' @param rapache_function
#' @param arguments
#'
#' @return
#' @export
#'
#' @examples
rapache <- function(rapache_function, arguments=list()) {
  if (is.environment('rapache'))
    do.call(rapache_function, arguments)
}



#' Check if endpoint is allowed/exists
#'
#' @param endpoint Character vector length two: package and function name
#' @param allowed_packages Character vector of allowed package endpoints
#'
#' @return
#'
#' @examples
endpoint_valid <- function(endpoint, packages) {


  # check if package is allowed
  if (!endpoint[1] %in% packages) {

    cat('Package', endpoint[1], 'is not an allowed package\n')
    rapache('setStatus', list(status=403L))
    return(FALSE)
  }

  # check if package exports function
  package_functions <- getNamespaceExports(endpoint[1])
  if (!endpoint[2] %in% package_functions) {

    cat('Function', endpoint[2], 'is not exported by package', endpoint[1], '\n')
    rapache('setStatus', list(status=404L))
    return(FALSE)
  }

  return(TRUE)
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


