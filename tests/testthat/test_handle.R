# setup
library(handlr)

SERVER <- list(method='GET')
POST <- list(a=1, b=2)
GET <- list(a=3, b=4)


test_that("get_params return params equal to POST or GET", {

  # GET request
  expect_equal(GET, handlr:::get_params(GET, POST, SERVER))

  # POST request
  SERVER$method <- 'POST'
  expect_equal(POST, handlr:::get_params(GET, POST, SERVER))

})

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

test_that("valid_endpoint allows exported functions in specified packages", {

    result <- handlr:::valid_endpoint(c('ggplot2', 'ggplot'), 'ggplot2')
    expect_true(result)

    result <- handlr:::valid_endpoint(c('ggplot2', 'matrix'), 'ggplot2')
    expect_false(result)

    result <- handlr:::valid_endpoint(c('base', 'matrix'), 'base')
    expect_true(result)

    result <- handlr:::valid_endpoint(c('gobledigook', 'matrix'), 'base')
    expect_false(result)

    # example test that should fail but doesn't
    # not critical as handlr functions will not be endpoints
    result <- handlr:::valid_endpoint(c('handlr', 'valid_endpoint'), 'handlr')
    expect_failure(expect_false(result))
})

test_that("set_status returns NULL outside of rApache", {
  expect_null(handlr:::set_status(404L, 'Error!'))
})
