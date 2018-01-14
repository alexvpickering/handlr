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

test_that("get_fun returns vector of package name and function", {

  SERVER$path_info <- '/package_name/function/'
  fun <- handlr:::get_fun(SERVER)
  expect_equal(fun[1], 'package_name')
  expect_equal(fun[2], 'function')

  SERVER$path_info <- '/package_name/function/IGNORE_THIS?a=1&b=2'
  fun <- handlr:::get_fun(SERVER)
  expect_equal(fun[1], 'package_name')
  expect_equal(fun[2], 'function')
})
