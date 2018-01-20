# setup
library(handlr)
context("authr integration")

con <- mongolite::mongo('users', 'test')
try(con$drop(), silent = TRUE)

email <- 'blah@gmail.com'
password <- '12345'
db <- 'test'


test_that("handle returns JWT from evaluation of authr::add_user", {

    response <- httr::POST(url = 'http://localhost:8005/api/authr/add_user',
               encode = 'form',
               body = list(email = email, password = password, db = db))

    expect_equal(response$status_code, 200L)

    jwt <- jose::jwt_decode_hmac(rawToChar(response$content), 'secret')
    expect_equal(jwt$email, email)

})

test_that('handle returns JWT from evaluation of authr::login_user', {

  response <- httr::POST(url = 'http://localhost:8005/api/authr/login_user',
                         encode = 'form',
                         body = list(email = email, password = password, db = db))

  expect_equal(response$status_code, 200L)

  jwt <- jose::jwt_decode_hmac(rawToChar(response$content), 'secret')
  expect_equal(jwt$email, email)

})

# cleanup
try(con$drop(), silent = TRUE)
