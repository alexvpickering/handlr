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
  error = function(e) {setStatus(500L); print(e)})
