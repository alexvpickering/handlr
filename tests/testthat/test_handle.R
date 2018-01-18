# setup
library(handlr)
context("Utilities for handle")

con <- mongolite::mongo('users', 'test')
try(con$drop(), silent = TRUE)

email <- 'blah@gmail.com'
password <- '12345'
db <- 'test'

test_that("validate_jwt returns claim or throws error if invalid secret/jwt", {

  claim <- jose::jwt_claim(user = "alex")
  secret <- 'real_secret'
  jwt <- jose::jwt_encode_hmac(claim, secret)
  req_headers <- list('authorization' = paste('Bearer', jwt))

  result <- handlr:::validate_jwt(req_headers, secret)
  expect_equal(claim, result)

  # fake secret
  expect_error(handlr:::validate_jwt(req_headers, 'fake_secret'))

  # wrong jwt
  req_headers$authorization <- 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ'
  expect_error(handlr:::validate_jwt(req_headers, secret))

})

test_that("get_params adds secret to functions with argument 'secret'", {

    req_data <- list(
    method = "POST",
    get    = NULL,
    post   = list(a=1, b=2)
  )

  secret <- 'secret'
  fun <- function(a, b) {}
  secret_fun <- function(a, b, secret) {}

  # non-secret function
  params <- get_params(req_data, fun, secret)
  expect_equal(req_data$post, params)

  # secret function
  expected <- req_data$post
  expected$secret <- secret

  params <- get_params(req_data, secret_fun, secret)
  expect_equal(expected, params)
})


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
