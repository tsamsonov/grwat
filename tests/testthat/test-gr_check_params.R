test_that("Default parameters are correct", {
  params = gr_get_params()
  expect_message(gr_check_params(params))
})

test_that("Unknown parameters are detected", {
  params = gr_get_params()
  params$new = -2
  expect_error(gr_check_params(params))
})

test_that("Missing parameters are detected", {
  params = gr_get_params()
  params$grad1 = NULL
  expect_error(gr_check_params(params))
})