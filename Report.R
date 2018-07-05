params <-
list(name = "Хопёр", file_tot = "Total.txt", file_sep = "AllGrWat.txt", 
    locale = "EN", year = 1978L, fixedyear = FALSE)

## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(include=FALSE)

## ---- include=TRUE-------------------------------------------------------
knitr::asis_output("# Input data")

## ---- include=TRUE-------------------------------------------------------
knitr::asis_output("Reanalysis data inside the basin")

## ---- include=TRUE, warning=FALSE, message=FALSE, out.height='7in'-------
pngfile = stringr::str_interp("${knitr::opts_knit$get('root.dir')}/${list.files('.', '*.png')[1]}")
knitr::include_graphics(pngfile)

## ---- include=TRUE-------------------------------------------------------
knitr::asis_output("# Hydrograph separation")

## ---- include=TRUE, results='asis', warning=FALSE, message=FALSE, fig.height=8, fig.width=11----
params$file_sep %>% 
  grwat::read_separation() %>% 
  grwat::plot_separation(layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE),
                         locale = params$locale,
                         pagebreak = TRUE)

## ---- include=TRUE-------------------------------------------------------
knitr::asis_output("# Interannual changes")

## ---- include=TRUE, results='asis', warning=FALSE, message=FALSE, fig.height=8, fig.width=11----
total = grwat::read_variables(params$file_tot)
tests = grwat::test_variables(total)
grwat::plot_variables(total, 
                      tests = tests,
                      layout = matrix(c(1,2,3,4), nrow=2, byrow=TRUE),
                      locale = params$locale,
                      pagebreak = TRUE)

## ---- include=TRUE-------------------------------------------------------
knitr::asis_output("# Long-term changes")

## ---- fig.height=8, fig.width=11, warning=FALSE, include = TRUE, results='asis', echo=FALSE, message=FALSE----
grwat::plot_minmonth(total, change_year = 1978, locale = params$locale, pagebreak = TRUE)

## ---- fig.height=8, fig.width=11, warning=FALSE, include = TRUE, results='asis', echo=FALSE, message=FALSE----
grwat::plot_periods(total,
                    tests = tests,
                    layout = matrix(1:8, nrow=4, byrow=TRUE),
                    locale = params$locale,
                    pagebreak = TRUE)

## ---- include=TRUE-------------------------------------------------------
knitr::asis_output("# Statistical tests")

## ---- include = TRUE, results='asis', warning=FALSE, message=FALSE-------
grwat::kable_tests(tests)

## ------------------------------------------------------------------------
tests$pvalues %>% writexl::write_xlsx(paste('pvalues_', params$name, '.xlsx', sep=''))

