# setup
library(handlr)

SERVER <- list(method='GET')


test_that("get_endpoint returns vector of package name and function", {

  SERVER$path_info <- '/package_name/function/'
  endpoint <- handlr:::get_endpoint(SERVER)
  expect_equal(endpoint[1], 'package_name')
  expect_equal(endpoint[2], 'function')

  SERVER$path_info <- '/package_name/function/IGNORE_THIS?a=1&b=2'
  endpoint <- handlr:::get_endpoint(SERVER)
  expect_equal(endpoint[1], 'package_name')
  expect_equal(endpoint[2], 'function')

})

test_that("endpoint_valid allows exported functions in specified packages", {

    result <- handlr:::endpoint_valid(c('ggplot2', 'ggplot'), 'ggplot2')
    expect_true(result)

    result <- handlr:::endpoint_valid(c('ggplot2', 'matrix'), 'ggplot2')
    expect_false(result)

    result <- handlr:::endpoint_valid(c('base', 'matrix'), 'base')
    expect_true(result)

    result <- handlr:::endpoint_valid(c('gobledigook', 'matrix'), 'base')
    expect_false(result)

    # example test that should fail but doesn't
    # not critical as handlr functions will not be endpoints
    result <- handlr:::endpoint_valid(c('handlr', 'endpoint_valid'), 'handlr')
    expect_failure(expect_false(result))
})

test_that("endpoint_open checks for jwt requirement", {

  endpoint <- c('ggplot2', 'ggplot')
  result <- handlr:::endpoint_open(endpoint, list(endpoint))
  expect_true(result)

  result <- handlr:::endpoint_open(endpoint, list(c('ggplot2', 'aes')))
  expect_false(result)

  result <- handlr:::endpoint_open(endpoint, list(c('ggplot2', NA)))
  expect_false(result)
})


test_that("validate_jwt returns claim or throws error if invalid secret", {

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

