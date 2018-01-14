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

    packages <- 'handlr'

    result <- handlr:::valid_endpoint(c('handlr', 'handle'), packages)
    expect_true(result)

    result <- handlr:::valid_endpoint(c('handlr', 'set_status'), packages)
    expect_false(result)

    result <- handlr:::valid_endpoint(c('handlr', 'ggplot'), packages)
    expect_false(result)

})
