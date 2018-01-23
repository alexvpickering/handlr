parse_post <- function(req_body, ctype) {
  # check for no data
  if (!length(req_body)) {
    return(list())
  }

  # strip title from header
  ctype <- sub("Content-Type: ?", "", ctype, ignore.case=TRUE)

  # invalid content type
  if (!length(ctype) || !nchar(ctype)) {
    stop("No Content-Type header found.")
  }

  if (grepl("x-www-form-urlencoded", ctype, fixed=TRUE)) {
    if (is.raw(req_body)) {
      return(webutils::parse_query(req_body))
    } else {
      return(as.list(req_body))
    }

  } else if (grepl("^application/json", ctype)) {
    if (is.raw(req_body)) {
      jsondata <- rawToChar(req_body)
    } else {
      jsondata <- req_body
    }
    if (!(is_valid <- jsonlite::validate(jsondata))) {
      stop("Invalid JSON was posted: ", attr(is_valid, "err"))
    }
    obj <- as.list(jsonlite::fromJSON(jsondata))

  } else {
    stop("POST body with unknown conntent type: ", ctype)
  }

  return(obj)
}
