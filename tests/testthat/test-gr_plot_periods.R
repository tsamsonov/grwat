data(spas) 
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Period plot has the correct content", {
  
  tests = gr_test_vars(vars)
  plt1 = suppressWarnings(gr_plot_periods(vars, Qygr, year = 1978))
  plt2 = suppressWarnings(gr_plot_periods(vars, Qygr, tests = TRUE))
  plt3 = suppressWarnings(gr_plot_periods(vars, Qspmax, Qygr, tests = tests))
  plt4 = suppressWarnings(gr_plot_periods(vars, Qygr, Qspmax, D10w1, Wsprngr,
                                          layout = matrix(1:4, nrow = 2),
                                          tests = tests))
  expect_type(plt1, 'list')
  expect_type(plt2, 'list')
  expect_type(plt3, 'list')
  expect_type(plt4, 'list')
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_s3_class(plt1[[1]], 'ggplot')
    expect_s3_class(plt2[[1]], 'ggplot')
    expect_s3_class(plt3[[1]], 'ggplot')
    expect_s3_class(plt4[[1]], 'ggplot')
  } else {
    expect_true(ggplot2::is_ggplot(plt1[[1]]))
    expect_true(ggplot2::is_ggplot(plt2[[1]]))
    expect_true(ggplot2::is_ggplot(plt3[[1]]))
    expect_true(ggplot2::is_ggplot(plt4[[1]]))
  }
  
  
})










