data(spas)

test_that("ACF ggplot has the correct content", {
  
  plt = gr_plot_acf(spas, 0.65)
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_s3_class(plt, 'ggplot')
    expect_type(plt, 'list')
    expect_equal(plt$layers[[6]]$data$x, 6)
    expect_equal(plt$layers[[4]]$data$y, 0.7)
  } else {
    expect_true(ggplot2::is_ggplot(plt))
    expect_type(plt, 'object')
    expect_equal(plt@layers[[6]]$data$x, 6)
    expect_equal(plt@layers[[4]]$data$y, 0.7)
  }
})