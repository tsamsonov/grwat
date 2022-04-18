grenv = new.env(parent = emptyenv())
grenv$loc = 'EN'

#' Set the language that is used for plotting
#'
#' @param locale Character string locale. Currently only English (`'EN'`) and Russian (`'RU'`) locales are supported. Defaults to `'EN'`.
#'
#' @export
#'
#' @example inst/examples/gr_set_locale.R
#' 
gr_set_locale <- function(locale = 'EN') {
  if (locale %in% c('EN', 'RU')) {
    grenv$loc = locale
  } else {
    warning(crayon::white$bgBlue$bold('grwat:'), ' ',
            crayon::white$italic(locale),
            " locale is not supported yet. Use 'EN' or 'RU' locale.")
  }
}