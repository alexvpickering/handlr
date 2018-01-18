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
    body <- parse_post(SERVER$raw, req_data$ctype)

    file_index <- vapply(body, function(x){isTRUE(is.list(x) && !inherits(x, "AsIs"))}, logical(1))

    req_data$files <- body[file_index]
    req_data$post  <- body[!file_index]
  }

  return(req_data)
}
