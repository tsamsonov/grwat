data(spas) 
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Variables plot has the correct content", {
  
  # plot one selected variable
  plt = suppressWarnings(gr_plot_vars(vars, Qygr))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # plot one selected variable
  plt = suppressWarnings(gr_plot_vars(vars, datestart))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  
  # plot two variables sequentially
  plt = suppressWarnings(gr_plot_vars(vars, date10w1, Wpol3))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  expect_equal(length(plt), 2)
  
  # plot four variables in matrix layout
  plt = suppressWarnings(gr_plot_vars(vars, Qmax, Qygr, date10w1, Wpol3,
               layout = matrix(1:4, nrow = 2, byrow = TRUE)))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  expect_equal(length(plt), 4)
  
  # add tests calculated on the fly (only plotted variables are tested)
  plt = suppressWarnings(gr_plot_vars(vars, Qmax, Qygr, date10w1, Wpol3,
               layout = matrix(1:4, nrow = 2, byrow = TRUE),
               tests = TRUE))
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  expect_equal(length(plt), 4)
  
  # calculate tests beforehand
  tests = gr_test_vars(vars)
  plt = suppressWarnings(
          gr_plot_vars(vars, date10w1, Wpol3, DaysThawWin, Qmaxpavs,
                       layout = matrix(1:4, nrow = 2, byrow = TRUE),
                       tests = tests)
  )
  expect_type(plt, 'list')
  expect_s3_class(plt[[1]], 'ggplot')
  expect_equal(length(plt), 4)
  
})