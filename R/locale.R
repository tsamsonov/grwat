grenv = new.env(parent = emptyenv())
grenv$loc = 'EN'

#' Set the language that is used for plotting
#' 
#' Run this function once at the beginning of the session. All plots will be labeled using the selected language.
#' 
#' Note to Linux users: the desired locale may not be installed on the system. A list of available locales can be obtained in bash terminal:
#' 
#' `locale -a`
#' 
#' Russian locale is `ru_RU.UTF-8`, and Ukrainian locale is `uk_UA.UTF-8`. If absent in the list, then install the desired locales by:
#' 
#' `sudo locale-gen ru_RU.UTF-8`
#' 
#' `sudo locale-gen uk_UA.UTF-8`
#' 
#' `sudo update-locale`
#' 
#' Then restart R session, and localization should work as expected.
#'
#' @param locale Character string locale. Currently only English (`'EN'`), Russian (`'RU'`) and Ukrainian (`'UA'`) locales are supported. More locales can be requested at issue on GitHub. Defaults to `'EN'`.
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @example inst/examples/gr_set_locale.R
#' 
gr_set_locale <- function(locale = 'EN') {
  if (locale %in% c('EN', 'RU', 'UA')) {
    grenv$loc = locale
  } else {
    warning(crayon::white$bgBlue$bold('grwat:'), ' ',
            crayon::white$italic(locale),
            " locale is not supported yet. Use 'EN', 'RU' or 'UA' locale.")
  }
}

gr_unescape <- function(labs) {
  for (i in seq_along(labs))
    if (is.character(labs[[i]]))
      labs[[i]] = suppressWarnings(stringi::stri_unescape_unicode(labs[[i]]))
  return(labs)
}