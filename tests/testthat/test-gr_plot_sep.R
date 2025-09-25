data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))

test_that("Period plot has the correct content", {
  
  # One year
  plt1 = suppressWarnings(gr_plot_sep(sep, 1978))
  # Two years
  plt2 = suppressWarnings(gr_plot_sep(sep, c(1978, 1989)))
  # Four years in a matrix layout
  plt3 = suppressWarnings(gr_plot_sep(sep, 1988:1991, layout = matrix(1:4, nrow = 2, byrow = TRUE))) 
  # Four years in a matrix layout with free Y scale
  plt3 = suppressWarnings(gr_plot_sep(sep, 1988:1991, layout = matrix(1:4, nrow = 2, byrow = TRUE), yfree = TRUE)) 
  # Add temperature
  plt4 = suppressWarnings(gr_plot_sep(sep, 1991, temp = TRUE))
  # Add precipitation
  plt5 = suppressWarnings(gr_plot_sep(sep, 1991, prec = TRUE))
  # Increase cumulative sum span for precipitation
  plt6 = suppressWarnings(gr_plot_sep(sep, 1991, prec = TRUE, span = 10))
  # Add both
  plt7 = suppressWarnings(gr_plot_sep(sep, 1991, temp = TRUE, prec = TRUE))
  
  expect_type(plt1, 'list')
  expect_type(plt2, 'list')
  expect_type(plt3, 'list')
  expect_type(plt4, 'list')
  expect_type(plt5, 'list')
  expect_type(plt6, 'list')
  expect_type(plt7, 'list')
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_s3_class(plt1[[1]], 'ggplot')
    expect_s3_class(plt2[[1]], 'ggplot')
    expect_s3_class(plt3[[1]], 'ggplot')
    expect_s3_class(plt4[[1]], 'ggplot')
    expect_s3_class(plt5[[1]], 'ggplot')
    expect_s3_class(plt6[[1]], 'ggplot')
    expect_s3_class(plt7[[1]], 'ggplot')
  } else {
    expect_true(ggplot2::is_ggplot(plt1[[1]]))
    expect_true(ggplot2::is_ggplot(plt2[[1]]))
    expect_true(ggplot2::is_ggplot(plt3[[1]]))
    expect_true(ggplot2::is_ggplot(plt4[[1]]))
    expect_true(ggplot2::is_ggplot(plt5[[1]]))
    expect_true(ggplot2::is_ggplot(plt6[[1]]))
    expect_true(ggplot2::is_ggplot(plt7[[1]]))
  }
  
})
