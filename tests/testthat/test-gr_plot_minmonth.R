data(spas)
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))

test_that("Minimum month plot has the correct content", {
  
  plt1 = gr_plot_minmonth(vars, tests = gr_test_vars(vars))
  plt2 = gr_plot_minmonth(vars, year = 1978)
  
  expect_type(plt1, 'list')
  expect_type(plt2, 'list')
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_s3_class(plt1[[1]], 'ggplot')
    expect_s3_class(plt1[[2]], 'ggplot')
    expect_equal(plt1[[1]][["plot_env"]][["periodtitle1_summer"]], "before 2000")
    
    expect_s3_class(plt2[[1]], 'ggplot')
    expect_s3_class(plt2[[2]], 'ggplot')
    expect_equal(plt2[[1]][["plot_env"]][["periodtitle1_summer"]], "before 1978")
  } else {
    expect_true(ggplot2::is_ggplot(plt1[[1]]))
    expect_true(ggplot2::is_ggplot(plt1[[2]]))
    expect_equal(plt1[[1]]@plot_env[["periodtitle1_summer"]], "before 2000")
    
    expect_true(ggplot2::is_ggplot(plt2[[1]]))
    expect_true(ggplot2::is_ggplot(plt2[[2]]))
    expect_equal(plt2[[1]]@plot_env[["periodtitle1_summer"]], "before 1978")
  }
})

