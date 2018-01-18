parse_req <- function(SERVER, GET) {

  # POST body to raw
  post <- if (SERVER$method == 'POST') {
    list(
      body  = rapache('receiveBin'),
      ctype = SERVER$headers_in[['content-type']]
    )
  }

  #collect request data from rapache
  req_data <- list(
    method = SERVER$method,
    get = GET,
    raw = post,
    accept = SERVER$headers_in[["accept"]]
  )

  return(req_data)
}
