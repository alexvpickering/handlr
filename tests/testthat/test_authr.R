# setup
library(handlr)
context("authr integration")

Sys.setenv(USERS_DB = 'test', JWT_SECRET = 'secret', EMAIL_VARS = '/var/www/R/email/vars.R', SEND_EMAIL = 'FALSE')

con <- mongolite::mongo('users', 'test')
try(con$drop(), silent = TRUE)

email <- 'blah@gmail.com'
password <- '12345'

responseToR <- function(response) {

  # convert back to JSON string
  json <- rawToChar(response$content)

  # convert to R string
  jsonlite::fromJSON(json)
}



test_that("handle returns JWT from evaluation of authr::register_user", {

    response <- httr::POST(url = 'http://localhost:8005/api/authr/register_user',
               encode = 'form',
               body = list(email = email, password = password))

    expect_equal(response$status_code, 200L)

    jwt <- jose::jwt_decode_hmac(responseToR(response), Sys.getenv('JWT_SECRET'))
    expect_equal(jwt$email, email)

})

test_that('handle returns JWT from evaluation of authr::login_user', {

  response <- httr::POST(url = 'http://localhost:8005/api/authr/login_user',
                         encode = 'form',
                         body = list(email = email, password = password))

  expect_equal(response$status_code, 200L)

  jwt <- jose::jwt_decode_hmac(responseToR(response), Sys.getenv('JWT_SECRET'))
  expect_equal(jwt$email, email)

})

# cleanup
Sys.unsetenv(c('USERS_DB', 'JWT_SECRET', 'EMAIL_VARS', 'SEND_EMAIL'))
try(con$drop(), silent = TRUE)
