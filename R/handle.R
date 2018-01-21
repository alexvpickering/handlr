#' Handles incomming rApache requests
#'
#' @param SERVER rApache variable.
#' @param GET rApache variable.
#' @param packages Vector of packages that api allows.
#' @param open Endpoints that are open to non-authenticated users.
#'    Specify as named list of vectors.
#'    for example: list(package_name = 'package_function')
#' @param timeout Time limit in seconds for function to execute.
#' @param rlimits named vector/list with rlimit values,
#'    for example: c(cpu = 60, fsize = 1e6).

#' @return
#' @export
#'
#' @examples
handle <- function(SERVER, GET, packages, open = 'all', timeout = 0, rlimits = NULL) {

  names(SERVER$headers_in) <- tolower(names(SERVER$headers_in))

  # CORS
  rapache('setHeader', header = "Access-Control-Allow-Origin", value = "*")

  # check if endpoint is allowed/exported
  endpoint <- get_endpoint(SERVER)
  validate_endpoint(endpoint, packages)

  # get claim if endpoint requires JSON web token
  claim <- if(!endpoint_open(endpoint, open)) {
    validate_jwt(SERVER$headers_in)
  }

  # get raw body
  SERVER$raw <- rapache('receiveBin')

  # parse request/get params
  req_data <- parse_req(SERVER, GET)
  params <- get_params(req_data)

  # get function to call (same as pkg::name)
  fun <- getExportedValue(endpoint$pkg, endpoint$fun)

  # call function in forked process with specified limits
  result <- sys::eval_safe(
    do.call(fun, params),
    timeout = timeout,
    rlimits = rlimits
  )

  cat(jsonlite::toJSON(result))
}


#' Puts parameters for endpoint function into a list.
#'
#'
#' @param req_data Result of call to \code{get_req}.
#'
#' @return Named list of parameters suplied in request.
#'
#' @examples
get_params <- function(req_data) {

  params <- switch(req_data$method,
                   'POST' = req_data$post,
                   'GET'  = req_data$get)

  if (is.null(params)) params <- list()
  return(params)

}


#' Validates JSON web token
#'
#' Error occurs if validation fails.
#'
#' @param req_headers List with request headers.
#'
#' @return List with claim.
#'
#' @examples
validate_jwt <- function(req_headers) {

  secret <- get_env('JWT_SECRET')

  jwt <- req_headers[['authorization']]
  jwt <- gsub('^Bearer ', '', jwt)

  jose::jwt_decode_hmac(jwt, secret)
}

#' Get environment variable.
#'
#' Throws error is environment variable is not defined.
#'
#' @param var_name Environment variable to get.
#' @return Value of environment variable.
#'
#' @examples
get_env <- function(var_name) {
  var <- Sys.getenv(var_name)
  if (var == '') stop(var_name, ' environment variable is not set.')
  return(var)
}


#' Check if endpoint is open
#'
#' @inheritParams validate_endpoint
#' @inheritParams handle
#'
#' @return
#'
#' @examples
endpoint_open <- function(endpoint, open='all') {
  if (open == 'all') return(TRUE)

  return(endpoint$fun %in% open[[endpoint$pkg]])
}


#' Helper function to permit testing.
#'
#' Calls rApache function if it can
#'
#' @param rapache_function Name of rApache function to evaluate.
#' @param ... Additional arguments to rApache function.
#'
#' @return
#'
#' @examples
rapache <- function(rapache_function, ...) {
  try(get(rapache_function)(...), silent=TRUE)
}



#' Check if endpoint is allowed/exists
#'
#' @param endpoint Named list with \code{pkg} and \code{fun} strings.
#' @param allowed_packages Character vector of allowed package endpoints
#'
#' @return
#'
#' @examples
validate_endpoint <- function(endpoint, packages) {

  # check if package is allowed
  if (!endpoint$pkg %in% packages) {
    rapache('setStatus', status = 403L)
    stop('Package ', endpoint$pkg, ' is not an allowed package\n')
  }

  # check if package exports function
  package_functions <- getNamespaceExports(endpoint$pkg)
  if (!endpoint$fun %in% package_functions) {
    rapache('setStatus', status = 404L)
    stop('Function ', endpoint$fun, ' is not exported by package ', endpoint$pkg, '.')
  }

}



#' Gets the package and function names from a path
#'
#' @param SERVER rApache variable: list
#'
#' @return Named list with 'pkg' for the package name and 'fun' the function name.
#' @keywords internal
#'
#' @examples
get_endpoint <- function(SERVER) {

  endpoint <- strsplit(SERVER$path_info, '/')[[1]]
  endpoint <- list(pkg = endpoint[2], fun = endpoint[3])
  return(endpoint)
}


