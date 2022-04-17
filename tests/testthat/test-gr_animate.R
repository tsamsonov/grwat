skip_on_cran()
skip_if_not_installed('gganimate')
skip_if_not_installed('transformr')
skip_if_not_installed('gifski')
skip_if_not_installed('png')

test_that("Hydrograph animation works", {
  
  data(spas)
  df = spas[spas$Date < as.Date('1960-01-01'), ]
  anim = gr_animate(df, plot = FALSE)
  
  expect_type(anim, 'character')
  expect_s3_class(anim, 'gif_image')
  
})