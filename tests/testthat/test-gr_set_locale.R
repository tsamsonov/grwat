test_that('Unknown locales are handled', {
  
  expect_warning(gr_set_locale('DE'))
  
})