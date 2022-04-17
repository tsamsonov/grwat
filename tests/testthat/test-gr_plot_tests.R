data(spas) 

sep = gr_separate(spas, params = gr_get_params(reg = 'Midplain'))
vars = suppressWarnings(gr_summarize(sep))
tests = suppressWarnings(gr_test_vars(vars))

test_that("Tests plot has the correct content", {
  
  plt = gr_plot_tests(tests, type = 'year')
  expect_type(plt, 'list')
  expect_s3_class(plt, 'ggplot')
  expect_gt(plt[["layers"]][[3]][["data"]][["xintercept"]], 1976)
  expect_lt(plt[["layers"]][[3]][["data"]][["xintercept"]], 1980)
  
})

