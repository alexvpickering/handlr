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

  # test for multipart
  if (grepl("multipart/form-data", ctype, fixed=TRUE)) {
    return(multipart(req_body, ctype))
    # test for url-encoded

  } else if (grepl("x-www-form-urlencoded", ctype, fixed=TRUE)) {
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
    if (!(is_valid <- validate(jsondata))) {
      stop("Invalid JSON was posted: ", attr(is_valid, "err"))
    }
    obj <- as.list(fromJSON(jsondata))

  } else {
    stop("POST body with unknown conntent type: ", ctype)
  }

  # Empty POST data
  if (is.null(obj))
    obj <- as.list(obj)

  if (!is.list(obj) || length(names(obj)) < length(obj)) {
    stop("JSON input should be a named list.")
  }

  return(lapply(obj, function(x) {
    if (isTRUE(is.atomic(x) && length(x) == 1)) {
      #primitives as expressions
      return(deparse_atomic(x))
    } else {
      return(I(x))
    }
  }))
}

# base::deparse() fucks up utf8 strings
deparse_atomic <- function(x) {
  if (is.character(x)) {
    str <- jsonlite::toJSON(x)
    str <- sub("^\\[", "c(", str)
    sub("\\]$", ")", str)
  } else {
    paste(deparse(x), collapse = "\n")
  }
}
