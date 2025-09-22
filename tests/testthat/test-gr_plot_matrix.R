data(spas) 
sep = gr_separate(spas)

test_that("Matrix ggplot has the correct content", {
  
  plt1 = suppressWarnings(gr_plot_matrix(sep, type = 'runoff'))
  plt2 = suppressWarnings(gr_plot_matrix(sep, type = 'season'))
  plt3 = suppressWarnings(gr_plot_matrix(sep, type = 'component'))
  
  if (packageVersion("ggplot2") < '4.0') {
    expect_type(plt1, 'list')
    expect_s3_class(plt1, 'ggplot')
    expect_s3_class(plt1$layers[[1]]$geom, 'GeomRaster')
    
    expect_type(plt2, 'list')
    expect_s3_class(plt2, 'ggplot')
    expect_s3_class(plt2$layers[[1]]$geom, 'GeomRaster')
    
    expect_type(plt3, 'list')
    expect_s3_class(plt3, 'ggplot')
    expect_s3_class(plt3$layers[[1]]$geom, 'GeomRaster')
  } else {
    expect_true(ggplot2::is_ggplot(plt1))
    expect_type(plt1, 'object')
    expect_equal(class(plt1@layers[[1]]$geom)[1], 'GeomRaster')

    expect_true(ggplot2::is_ggplot(plt2))
    expect_type(plt2, 'object')
    expect_equal(class(plt2@layers[[1]]$geom)[1], 'GeomRaster')
    
    expect_true(ggplot2::is_ggplot(plt3))
    expect_type(plt3, 'object')
    expect_equal(class(plt3@layers[[1]]$geom)[1], 'GeomRaster')
  }
  
})