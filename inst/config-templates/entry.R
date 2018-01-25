# read R environmental variables
# see e.g. authr .Renviron template authr::open_templates()
readRenviron('path/to/.Renviron')

packages <- 'authr'
open <- list(authr = c('add_user',
                       'login_user',
                       'forgot_password',
                       'reset_password'))

tryCatch(
  handlr::handle(SERVER, GET, packages, open),

  error = function(e) {
    e <- e$message

    # set status with error messages e.g. '403 Package not allowed'
    if (grepl('^\\d{3} ', e)) {
      setStatus(substring(e, 0, 3))
      cat(substring(e, 5))

    } else {
      setStatus(400L); cat(e)
    }
  }
)
