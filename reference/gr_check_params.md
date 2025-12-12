# Check the correctness of parameters list for separating

Check the correctness of parameters list for separating

## Usage

``` r
gr_check_params(params, df = NULL)
```

## Arguments

- params:

  `list` of separation parameters, as returned by
  [`gr_get_params()`](gr_get_params.md) function

- df:

  `data.frame` with four columns: date, runoff, temperature,
  precipitation, as required by [`gr_separate()`](gr_separate.md).
  Required when params is a `list` of parameter `list`s. Defaults to
  `NULL`.

## Value

stops the execution if anything is wrong and prints the exact reason of
the error. Otherwise prints the message that everything is OK

## Examples

``` r
library(grwat)

# example Spas-Zagorye data is included with grwat package
data(spas)

params = gr_get_params(reg = 'center')

gr_check_params(params)
#> grwat: parameters list and types are OK

# set the unknown parameter
params$new = -2

# use try if you do not want to stop at error
try(gr_check_params(params))
#> Error in gr_check_params(params) : 
#>   grwat: new parameter(s) not known. Please use gr_get_params() result as a template.

# remove wrong parameter
params$new = NULL

# remove right parameter
params$grad1 = NULL
try(gr_check_params(params))
#> Error in gr_check_params(params) : 
#>   grwat: grad1 parameter(s) needed. Please use gr_get_params() result as a template.

# reset
params = gr_get_params(reg = 'center')

sep = gr_separate(spas, params, debug = TRUE)
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
#> Warning: grwat: 1974 years were not separated. Check the input data for possible errors. Use gr_get_gaps() and gr_fill_gaps() functions to detect and fill missing data.
#> Warning: grwat: 2002, 2014, 2019 years were processed with jittered parameters
parlist = attributes(sep)$params

parlist[['2002']]$grad1 = 4

# if the parlist is used for separation
# then data frame must be specified
try(gr_check_params(parlist))
#> Error in gr_check_params(parlist) : 
#>   grwat: df parameter is needed, because params is a list of lists.

gr_check_params(parlist, spas)
#> grwat: data frame is correct
#> grwat: parameters list and types are OK

# grad parameter is not known
parlist[['2002']]$grad = 4
try(gr_check_params(parlist, spas))
#> grwat: data frame is correct
#> Error in gr_check_params(parlist, spas) : 
#>   grwat: grad parameter(s) not known. Please use gr_get_params() result as a template.

# remove wrong parameter
parlist[['2002']]$grad = NULL

# remove year
parlist[['2002']] = NULL
try(gr_check_params(parlist, spas))
#> grwat: data frame is correct
#> Error in gr_check_params(parlist, spas) : 
#>   grwat: the length of parameters list must be equal to 1 or to the number of years in the data

parlist[['2002']] = parlist[['2001']]
gr_check_params(parlist, spas)
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
```
