# setup
library(handlr)


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
