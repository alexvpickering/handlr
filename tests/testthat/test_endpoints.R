# setup
library(handlr)

SERVER <- list(method='GET')

test_that("get_endpoint returns vector of package name and function", {

  SERVER$path_info <- '/package_name/function/'
  endpoint <- handlr:::get_endpoint(SERVER)
  expect_equal(endpoint$pkg, 'package_name')
  expect_equal(endpoint$fun, 'function')

  SERVER$path_info <- '/package_name/function/IGNORE_THIS?a=1&b=2'
  endpoint <- handlr:::get_endpoint(SERVER)
  expect_equal(endpoint$pkg, 'package_name')
  expect_equal(endpoint$fun, 'function')

})

test_that("validate_endpoint allows exported functions in specified packages", {

  result <- handlr:::validate_endpoint(list(pkg = 'ggplot2', fun = 'ggplot'), 'ggplot2')
  expect_null(result)

  expect_error(
    handlr:::validate_endpoint(list(pkg = 'ggplot2', fun = 'matrix'), 'ggplot2'),
    'not exported by package')


  result <- handlr:::validate_endpoint(list(pkg = 'base', fun = 'matrix'), 'base')
  expect_null(result)

  expect_error(
    handlr:::validate_endpoint(list(pkg = 'gobledigook', fun = 'matrix'), 'base'),
    'not an allowed package')

  # example test that should fail but doesn't
  # not critical as handlr functions will not be endpoints
  expect_failure(expect_error(
    handlr:::validate_endpoint(list(pkg = 'handlr', fun = 'validate_endpoint'), 'handlr'),
    'not exported by package'))
})

test_that("endpoint_open checks for jwt requirement", {

  endpoint <- list(pkg = 'ggplot2', fun = 'ggplot')
  result <- handlr:::endpoint_open(endpoint, list(ggplot2 = 'ggplot'))
  expect_true(result)

  result <- handlr:::endpoint_open(endpoint, list(ggplot2 = 'aes'))
  expect_false(result)

  result <- handlr:::endpoint_open(endpoint, list(ggplot2 = NA))
  expect_false(result)
})
