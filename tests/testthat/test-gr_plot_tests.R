data(spas) 

sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
vars = suppressWarnings(gr_summarize(sep))
tests = suppressWarnings(gr_test_vars(vars))

test_that("Tests plot has the correct content", {
  
  plt = gr_plot_tests(tests, type = 'year')
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_type(plt, 'list')
    expect_s3_class(plt, 'ggplot')
  } else {
    expect_type(plt, 'object')
    expect_true(ggplot2::is_ggplot(plt))
  }
  
  
  # TODO: need stable tests data for such check
  # expect_gte(plt[["layers"]][[3]][["data"]][["xintercept"]], 1975) 
  # expect_lte(plt[["layers"]][[3]][["data"]][["xintercept"]], 1980)
  
})

