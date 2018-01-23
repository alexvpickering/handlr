parse_req <- function(SERVER, GET) {

  #collect request data from rapache
  req_data <- list(
    method = SERVER$method,
    get    = GET,
    raw    = SERVER$raw,
    accept = SERVER$headers_in[["accept"]],
    ctype  = SERVER$headers_in[['content-type']]
  )

  # POST body to raw
  if (SERVER$method == 'POST') {
    req_data$post <- parse_post(SERVER$raw, req_data$ctype)
  }

  return(req_data)
}
