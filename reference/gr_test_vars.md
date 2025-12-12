# Test hydrograph changes

Use this function to test interannual changes or hydrograph separation
variables returned by [`gr_summarize()`](gr_summarize.md). Pettitt test
is used to detect the change year — i.e. the year which divides the time
series into the statistically most differing samples. Student (Welch)
and Fisher tests are used to estimate the significance of mean and
variance differences of these samples. Theil-Sen test calculates the
trend slope value. Mann-Kendall test is performed to reveal the
significance of the trend.

## Usage

``` r
gr_test_vars(df, ..., year = NULL, exclude = NULL)
```

## Arguments

- df:

  `data.frame` as produced by [`gr_summarize()`](gr_summarize.md)
  function.

- ...:

  Names of the tested variables (quoted).

- year:

  Integer value of year used to divide series in two samples compared by
  Student and Fisher tests. Defaults to `NULL` which means that the year
  is calculated automatically by Pettitt test.

- exclude:

  Integer vector of years to be excluded from tests.

## Value

`list` of testing results with following elements:

|              |                                                                                                 |
|--------------|-------------------------------------------------------------------------------------------------|
| **Element**  | **Description**                                                                                 |
| `ptt`        | Pettitt tests for change year                                                                   |
| `mkt`        | Mann-Kendall test for trend significance                                                        |
| `tst`        | Theil-Sen test for slope estimation                                                             |
| `ts_fit`     | Theil-Sen linear model fit                                                                      |
| `tt`         | Student (Welch) test for significance of mean differences between two periods                   |
| `ft`         | Fisher test for significance of variance differences between two periods                        |
| `year`       | Integer value of year used to divide series in two samples compared by Student and Fisher tests |
| `maxval`     | Maximum value for the variable along the full time series                                       |
| `fixed_year` | Boolean `TRUE` or `FALSE` value indicating if the year was fixed                                |
| `pvalues`    | p-values of all tests summarized as a single table for all variables                            |

## Details

Number of observations formally required for various tests: Pettitt \>
0, Mann-Kendall \> 2, Theil-Sen \> 1, Student \> 1, Fisher \> 1.

## Examples

``` r
library(grwat)

data(spas) # example Spas-Zagorye data is included with grwat package

# separate
sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
#> grwat: data frame is correct
#> grwat: parameters list and types are OK

# summarize from 1965 to 1990
vars = gr_summarize(sep, 1965, 1990)
#> Warning: There were 4 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `Dspstart = min(.data$Date[which(.data$Qspri > 0)])`.
#> ℹ In group 10: `Year1 = 1974`.
#> Caused by warning in `min.default()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.

# test all variables
tests = gr_test_vars(vars)

# view Pettitt test for Qygr
tests$ptt$Qygr
#> 
#>  Pettitt's test for single change-point detection
#> 
#> data:  vl[vl_cmp]
#> U* = 156, p-value = 0.0002504
#> alternative hypothesis: two.sided
#> sample estimates:
#> probable change point at time K 
#>                              12 
#> 

# view Fisher test for Q30s
tests$ft$Q30s
#> 
#>  F test to compare two variances
#> 
#> data:  d1 and d2
#> F = 0.17155, num df = 9, denom df = 14, p-value = 0.0117
#> alternative hypothesis: true ratio of variances is not equal to 1
#> 95 percent confidence interval:
#>  0.05345339 0.65153115
#> sample estimates:
#> ratio of variances 
#>           0.171548 
#> 

# test only Qygr and Q30s using 1978 as fixed year and excluding 1988-1991 yrs
gr_test_vars(vars, Qygr, Q30s, year = 1978, exclude = 1981:1983)
#> $ptt
#> $ptt$Qygr
#> 
#>  Pettitt's test for single change-point detection
#> 
#> data:  vl[vl_cmp]
#> U* = 120, p-value = 0.0008517
#> alternative hypothesis: two.sided
#> sample estimates:
#> probable change point at time K 
#>                              12 
#> 
#> 
#> $ptt$Q30s
#> 
#>  Pettitt's test for single change-point detection
#> 
#> data:  vl[vl_cmp]
#> U* = 117, p-value = 0.001249
#> alternative hypothesis: two.sided
#> sample estimates:
#> probable change point at time K 
#>                              11 
#> 
#> 
#> 
#> $mkt
#> $mkt$Qygr
#> 
#>  Mann-Kendall trend test
#> 
#> data:  vl[vl_cmp]
#> z = 4.85, n = 22, p-value = 1.234e-06
#> alternative hypothesis: true S is not equal to 0
#> sample estimates:
#>            S         varS          tau 
#>  173.0000000 1257.6666667    0.7489177 
#> 
#> 
#> $mkt$Q30s
#> 
#>  Mann-Kendall trend test
#> 
#> data:  vl[vl_cmp]
#> z = 4.7109, n = 22, p-value = 2.466e-06
#> alternative hypothesis: true S is not equal to 0
#> sample estimates:
#>           S        varS         tau 
#>  168.000000 1256.666667    0.728852 
#> 
#> 
#> 
#> $tst
#> $tst$Qygr
#> 
#>  Sen's slope
#> 
#> data:  df.theil[[2]][fltr]
#> z = 4.85, n = 22, p-value = 1.234e-06
#> alternative hypothesis: true z is not equal to 0
#> 95 percent confidence interval:
#>  0.4064579 0.6672221
#> sample estimates:
#> Sen's slope 
#>   0.5231335 
#> 
#> 
#> $tst$Q30s
#> 
#>  Sen's slope
#> 
#> data:  df.theil[[2]][fltr]
#> z = 4.7109, n = 22, p-value = 2.466e-06
#> alternative hypothesis: true z is not equal to 0
#> 95 percent confidence interval:
#>  0.3457500 0.5597037
#> sample estimates:
#> Sen's slope 
#>       0.457 
#> 
#> 
#> 
#> $ts_fit
#> $ts_fit$Qygr
#> 
#> Call:
#> mblm::mblm(formula = eval(frml), dataframe = df.theil[fltr, ], 
#>     repeated = FALSE)
#> 
#> Coefficients:
#> (Intercept)        Year1  
#>    -826.781        0.424  
#> 
#> 
#> $ts_fit$Q30s
#> 
#> Call:
#> mblm::mblm(formula = eval(frml), dataframe = df.theil[fltr, ], 
#>     repeated = FALSE)
#> 
#> Coefficients:
#> (Intercept)        Year1  
#>   -712.0388       0.3642  
#> 
#> 
#> 
#> $tt
#> $tt$Qygr
#> 
#>  Welch Two Sample t-test
#> 
#> data:  d1 and d2
#> t = -6.1293, df = 13.413, p-value = 3.146e-05
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -8.792203 -4.220127
#> sample estimates:
#> mean of x mean of y 
#>  8.539897 15.046061 
#> 
#> 
#> $tt$Q30s
#> 
#>  Welch Two Sample t-test
#> 
#> data:  d1 and d2
#> t = -6.8192, df = 12.971, p-value = 1.241e-05
#> alternative hypothesis: true difference in means is not equal to 0
#> 95 percent confidence interval:
#>  -7.308626 -3.791274
#> sample estimates:
#> mean of x mean of y 
#>  5.551333 11.101283 
#> 
#> 
#> 
#> $ft
#> $ft$Qygr
#> 
#>  F test to compare two variances
#> 
#> data:  d1 and d2
#> F = 0.30259, num df = 11, denom df = 9, p-value = 0.06579
#> alternative hypothesis: true ratio of variances is not equal to 1
#> 95 percent confidence interval:
#>  0.07734699 1.08565221
#> sample estimates:
#> ratio of variances 
#>          0.3025872 
#> 
#> 
#> $ft$Q30s
#> 
#>  F test to compare two variances
#> 
#> data:  d1 and d2
#> F = 0.27016, num df = 11, denom df = 9, p-value = 0.04493
#> alternative hypothesis: true ratio of variances is not equal to 1
#> 95 percent confidence interval:
#>  0.06905736 0.96929782
#> sample estimates:
#> ratio of variances 
#>          0.2701575 
#> 
#> 
#> 
#> $year
#> Qygr Q30s 
#> 1978 1978 
#> 
#> $maxval
#> $maxval$Qygr
#> [1] 22.0631
#> 
#> $maxval$Q30s
#> [1] 15.41
#> 
#> 
#> $fixed_year
#> [1] TRUE
#> 
#> $pvalues
#>   N                                    Variable Change.Year   Trend      M1
#> 1 1 Mean annual groundwater ("baseflow") runoff        1978 0.42401 8.53990
#> 2 2       Minimum 30-day averaged summer runoff        1978 0.36417 5.55133
#>         M2 MeanRatio     sd1     sd2 sdRatio Mann.Kendall Pettitt Student
#> 1 15.04606      76.2 1.65010 2.99975    81.8            0 0.00085   3e-05
#> 2 11.10128     100.0 1.20858 2.32523    92.4            0 0.00125   1e-05
#>    Fisher
#> 1 0.06579
#> 2 0.04493
#> 
```
