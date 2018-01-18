#' Handles incomming rApache requests
#'
#' @param SERVER rApache variable.
#' @param GET rApache variable.
#' @param packages Vector of packages that api allows.
#' @param open Endpoints that are open to non-authenticated users.
#'    Specify as named list of vectors.
#'    for example: list(package_name = 'package_function')
#' @param secret string or raw vector needed to encode/decode JSON web token.
#'    Gets passed along to endpoints with argument 'secret'.
#' @param timeout Time limit in seconds for function to execute.
#' @param rlimits named vector/list with rlimit values,
#'    for example: c(cpu = 60, fsize = 1e6).

#' @return
#' @export
#'
#' @examples
handle <- function(SERVER, GET, packages, open = 'all', secret = NULL,
                   timeout = 0, rlimits = NULL) {

  names(SERVER$headers_in) <- tolower(names(SERVER$headers_in))


  # check if endpoint is allowed/exported
  endpoint <- get_endpoint(SERVER)
  validate_endpoint(endpoint, packages)

  # get claim if endpoint requires JSON web token
  claim <- if(!endpoint_open(endpoint, open)) {
    validate_jwt(SERVER$headers_in, secret)
  }

  # parse request
  req_data <- parse_req(SERVER, GET)
  rapache('setHeader', header = "X-Powered-By" ,value = "rApache")

  # get function to call (same as pkg::name)
  what <- getExportedValue(endpoint$pkg, endpoint$fun)

  # call function in forked process with specified limits
  result <- sys::eval_safe(
    do.call(what, params),
    timeout = timeout,
    rlimits = rlimits
  )

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

  if (is.null(secret)) {
    rapache('setStatus', status = 500L)
    stop("argument 'secret' must be a string or raw vector")
  }

  jwt <- req_headers[['authorization']]
  jwt <- gsub('^Bearer ', '', jwt)

  jose::jwt_decode_hmac(jwt, secret)
}


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
#' @export
#'
#' @examples
rapache <- function(rapache_function, ...) {
  try(get(rapache_function)(...), silent=TRUE)
}



#' Check if endpoint is allowed/exists
#'
#' @param endpoint Character vector length two: package and function name
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


