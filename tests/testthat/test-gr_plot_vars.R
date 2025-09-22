data(spas) 
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Variables plot has the correct content", {
  
  # plot one selected variable
  plt1 = suppressWarnings(gr_plot_vars(vars, Qygr))
  # plot one selected variable
  plt2 = suppressWarnings(gr_plot_vars(vars, Dspstart))
  # plot two variables sequentially
  plt3 = suppressWarnings(gr_plot_vars(vars, D10w1, Wsprngr))
  # plot four variables in matrix layout
  plt4 = suppressWarnings(gr_plot_vars(vars, Qspmax, Qygr, D10w1, Wsprngr,
                                       layout = matrix(1:4, nrow = 2, byrow = TRUE)))
  # add tests calculated on the fly (only plotted variables are tested)
  plt5 = suppressWarnings(gr_plot_vars(vars, Qspmax, Qygr, D10w1, Wsprngr,
                                       layout = matrix(1:4, nrow = 2, byrow = TRUE),
                                       tests = TRUE))
  # calculate tests beforehand
  tests = gr_test_vars(vars)
  plt6 = suppressWarnings(
    gr_plot_vars(vars, D10w1, Wsprngr, Nthw, Qrnmax,
                 layout = matrix(1:4, nrow = 2, byrow = TRUE),
                 tests = tests)
  )
  
  expect_type(plt1, 'list')
  expect_type(plt2, 'list')
  expect_type(plt3, 'list')
  expect_type(plt4, 'list')
  expect_type(plt5, 'list')
  expect_type(plt6, 'list')
  expect_equal(length(plt3), 2)
  expect_equal(length(plt4), 4)
  expect_equal(length(plt5), 4)
  expect_equal(length(plt6), 4)
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_s3_class(plt1[[1]], 'ggplot')
    expect_s3_class(plt2[[1]], 'ggplot')
    expect_s3_class(plt3[[1]], 'ggplot')
    expect_s3_class(plt4[[1]], 'ggplot')
    expect_s3_class(plt5[[1]], 'ggplot')
    expect_s3_class(plt6[[1]], 'ggplot')
  } else {
    expect_true(ggplot2::is_ggplot(plt1[[1]]))
    expect_true(ggplot2::is_ggplot(plt2[[1]]))
    expect_true(ggplot2::is_ggplot(plt3[[1]]))
    expect_true(ggplot2::is_ggplot(plt4[[1]]))
    expect_true(ggplot2::is_ggplot(plt5[[1]]))
    expect_true(ggplot2::is_ggplot(plt6[[1]]))
  }
  
  
  
})