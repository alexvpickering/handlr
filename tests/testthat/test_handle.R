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

test_that("get_fun returns second relative endpoint", {

  SERVER$path_info <- '/handlr/function/B/Z/234234?a=1&b=2'
  fun <- handlr:::get_fun(SERVER)
  expect_equal(fun, 'function')
})
