library(handlr)
context("Utilities for handle")


test_that("validate_jwt returns claim or throws error if invalid secret/jwt", {

  Sys.setenv(JWT_SECRET = 'secret')
  claim <- jose::jwt_claim(user = "alex")
  jwt <- jose::jwt_encode_hmac(claim, 'secret')
  req_headers <- list('authorization' = paste('Bearer', jwt))

  result <- handlr:::validate_jwt(req_headers)
  expect_equal(claim, result)

  # fake secret
  Sys.setenv(JWT_SECRET = 'fake_secret')
  expect_error(handlr:::validate_jwt(req_headers))

  # wrong jwt
  req_headers$authorization <- 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiYWRtaW4iOnRydWV9.TJVA95OrM7E2cBab30RMHrHDcEfxjoYZgeFONFh7HgQ'
  expect_error(handlr:::validate_jwt(req_headers, secret))

})
