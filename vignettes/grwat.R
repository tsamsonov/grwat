## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(grwat)
library(dplyr)
library(ggplot2)
library(lubridate)

data(spas)
head(spas)

## -----------------------------------------------------------------------------
Qbase = gr_baseflow(spas$Q, method = 'lynehollick', a = 0.925, passes = 3)
head(Qbase)

## -----------------------------------------------------------------------------
# Calculate baseflow using Jakeman approach
hdata = spas %>% 
  mutate(Qbase = gr_baseflow(Q, method = 'jakeman'))

# Visualize for 2020 year
ggplot(hdata) +
  geom_area(aes(Date, Q), fill = 'steelblue', color = 'black') +
  geom_area(aes(Date, Qbase), fill = 'orangered', color = 'black') +
  scale_x_date(limits = c(ymd(19800101), ymd(19801231)))

## -----------------------------------------------------------------------------
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
head(sep)

## ----warning=FALSE------------------------------------------------------------
gr_plot_sep(sep, years = c(1978, 1989))

## ----warning=FALSE------------------------------------------------------------
vars = gr_summarize(sep)
head(vars)

## -----------------------------------------------------------------------------
gr_plot_vars(vars, Qygr)
gr_plot_vars(vars, D10w1, Wsprngr, Nthw, Qrnmax, tests = TRUE,
             layout = matrix(1:4, nrow = 2, byrow = TRUE)) 

