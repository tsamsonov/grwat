# Tabular representation of tests

This function is used to represent the results of
[`gr_test_vars()`](gr_test_vars.md) in a tabular form. Used mainly in
[`gr_report()`](gr_report.md), but can be used for your own purposes.

## Usage

``` r
gr_kable_tests(tests, format = "html")
```

## Arguments

- tests:

  `list` of tests as returned by [`gr_test_vars()`](gr_test_vars.md)
  function.

- format:

  Character string encoding the type of output. Currently `'html'` only
  is supported.

## Value

HTML table as returned by
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) function.

## Examples

``` r
if (require("kableExtra")) {
  
  library(grwat)
  
  data(spas) # example Spas-Zagorye data is included with grwat package
  
  # separate
  sep = gr_separate(spas, params = gr_get_params(reg = 'center'))
  
  # summarize from 1965 to 1990
  vars = gr_summarize(sep, 1965, 1990)
  
  # test all variables
  tests = gr_test_vars(vars)
  
  # kable tests
  gr_kable_tests(tests)

}
#> Loading required package: kableExtra
#> 
#> Attaching package: ‘kableExtra’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     group_rows
#> grwat: data frame is correct
#> grwat: parameters list and types are OK
#> Warning: There were 4 warnings in `dplyr::summarise()`.
#> The first warning was:
#> ℹ In argument: `Dspstart = min(.data$Date[which(.data$Qspri > 0)])`.
#> ℹ In group 10: `Year1 = 1974`.
#> Caused by warning in `min.default()`:
#> ! no non-missing arguments to min; returning Inf
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.
#> <table class="table table-striped" style="margin-left: auto; margin-right: auto;">
#> <caption>p-values of statistical criteria</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:right;"> N </th>
#>    <th style="text-align:left;"> Variable </th>
#>    <th style="text-align:right;"> Change.Year </th>
#>    <th style="text-align:left;"> Trend </th>
#>    <th style="text-align:left;"> M1 </th>
#>    <th style="text-align:left;"> M2 </th>
#>    <th style="text-align:left;"> MeanRatio </th>
#>    <th style="text-align:right;"> sd1 </th>
#>    <th style="text-align:right;"> sd2 </th>
#>    <th style="text-align:left;"> sdRatio </th>
#>    <th style="text-align:left;"> Mann.Kendall </th>
#>    <th style="text-align:left;"> Pettitt </th>
#>    <th style="text-align:left;"> Student </th>
#>    <th style="text-align:left;"> Fisher </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:right;"> 1 </td>
#>    <td style="text-align:left;"> Annual runoff volume </td>
#>    <td style="text-align:right;"> 1978 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.01847</span> </td>
#>    <td style="text-align:left;"> 0.55284 </td>
#>    <td style="text-align:left;"> 0.79601 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">44</span> </td>
#>    <td style="text-align:right;"> 0.16174 </td>
#>    <td style="text-align:right;"> 0.19032 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">17.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00042</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00821</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00219</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.59732</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 2 </td>
#>    <td style="text-align:left;"> Spring flood runoff volume (w/o groundwater) </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">2e-05</span> </td>
#>    <td style="text-align:left;"> 0.23398 </td>
#>    <td style="text-align:left;"> 0.19666 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-16</span> </td>
#>    <td style="text-align:right;"> 0.13441 </td>
#>    <td style="text-align:right;"> 0.11952 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-11.1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.98137</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.85423</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.59219</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.63668</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 3 </td>
#>    <td style="text-align:left;"> Annual groundwater runoff volume </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.01332</span> </td>
#>    <td style="text-align:left;"> 0.26059 </td>
#>    <td style="text-align:left;"> 0.4575 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">75.6</span> </td>
#>    <td style="text-align:right;"> 0.04456 </td>
#>    <td style="text-align:right;"> 0.10032 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">125.1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00025</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.01452</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 4 </td>
#>    <td style="text-align:left;"> Spring flood runoff volume (with groundwater) </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.00132</span> </td>
#>    <td style="text-align:left;"> 0.29099 </td>
#>    <td style="text-align:left;"> 0.27483 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-5.6</span> </td>
#>    <td style="text-align:right;"> 0.15648 </td>
#>    <td style="text-align:right;"> 0.14758 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-5.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.59115</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.84155</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.74904</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 5 </td>
#>    <td style="text-align:left;"> Rain flood runoff volume (w/o groundwater) </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.00446</span> </td>
#>    <td style="text-align:left;"> 0.04168 </td>
#>    <td style="text-align:left;"> 0.11617 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">178.7</span> </td>
#>    <td style="text-align:right;"> 0.04870 </td>
#>    <td style="text-align:right;"> 0.09605 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">97.2</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00351</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.03157</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02034</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.03782</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 6 </td>
#>    <td style="text-align:left;"> Rain flood runoff volume (with groundwater) </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.01086</span> </td>
#>    <td style="text-align:left;"> 0.12321 </td>
#>    <td style="text-align:left;"> 0.30311 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">146</span> </td>
#>    <td style="text-align:right;"> 0.09672 </td>
#>    <td style="text-align:right;"> 0.16799 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">73.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.0019</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00821</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00291</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.08734</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 7 </td>
#>    <td style="text-align:left;"> Thaw flood runoff volume (w/o groundwater) </td>
#>    <td style="text-align:right;"> 1983 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.00033</span> </td>
#>    <td style="text-align:left;"> 0.02354 </td>
#>    <td style="text-align:left;"> 0.0153 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-35</span> </td>
#>    <td style="text-align:right;"> 0.02217 </td>
#>    <td style="text-align:right;"> 0.02264 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">2.1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.18311</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.18826</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.40725</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.88128</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 8 </td>
#>    <td style="text-align:left;"> Thaw flood runoff volume (with groundwater) </td>
#>    <td style="text-align:right;"> 1984 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.00011</span> </td>
#>    <td style="text-align:left;"> 0.07835 </td>
#>    <td style="text-align:left;"> 0.06175 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-21.2</span> </td>
#>    <td style="text-align:right;"> 0.03607 </td>
#>    <td style="text-align:right;"> 0.04230 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">17.3</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.87014</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.62829</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.38174</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.56</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 9 </td>
#>    <td style="text-align:left;"> Winter groundwater runoff volume </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.00258</span> </td>
#>    <td style="text-align:left;"> 0.06247 </td>
#>    <td style="text-align:left;"> 0.10656 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">70.6</span> </td>
#>    <td style="text-align:right;"> 0.01517 </td>
#>    <td style="text-align:right;"> 0.02121 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">39.8</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00029</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00025</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.2927</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 10 </td>
#>    <td style="text-align:left;"> Winter low flow runoff volume </td>
#>    <td style="text-align:right;"> 1978 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.00222</span> </td>
#>    <td style="text-align:left;"> 0.07786 </td>
#>    <td style="text-align:left;"> 0.12633 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">62.3</span> </td>
#>    <td style="text-align:right;"> 0.02219 </td>
#>    <td style="text-align:right;"> 0.02980 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">34.3</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.01246</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00264</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00013</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.33836</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 11 </td>
#>    <td style="text-align:left;"> Summer groundwater runoff volume </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.00851</span> </td>
#>    <td style="text-align:left;"> 0.12988 </td>
#>    <td style="text-align:left;"> 0.2591 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">99.5</span> </td>
#>    <td style="text-align:right;"> 0.04265 </td>
#>    <td style="text-align:right;"> 0.09220 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">116.2</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00076</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00017</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.01963</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 12 </td>
#>    <td style="text-align:left;"> Summer low flow runoff volume </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.0117</span> </td>
#>    <td style="text-align:left;"> 0.16432 </td>
#>    <td style="text-align:left;"> 0.35865 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">118.3</span> </td>
#>    <td style="text-align:right;"> 0.08927 </td>
#>    <td style="text-align:right;"> 0.18732 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">109.8</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">5e-05</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00569</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.0028</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02438</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 13 </td>
#>    <td style="text-align:left;"> Mean annual runoff </td>
#>    <td style="text-align:right;"> 1978 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.57807</span> </td>
#>    <td style="text-align:left;"> 17.70255 </td>
#>    <td style="text-align:left;"> 25.23654 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">42.6</span> </td>
#>    <td style="text-align:right;"> 5.45418 </td>
#>    <td style="text-align:right;"> 5.51354 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">1.1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00035</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00982</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00228</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.97828</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 14 </td>
#>    <td style="text-align:left;"> First date of a spring flood </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.26136</span> </td>
#>    <td style="text-align:left;"> 26-Mar </td>
#>    <td style="text-align:left;"> 16-Mar </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-10</span> </td>
#>    <td style="text-align:right;"> 7.00000 </td>
#>    <td style="text-align:right;"> 13.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">85.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.22276</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.3627</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.04855</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.28875</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 15 </td>
#>    <td style="text-align:left;"> Mean annual groundwater ("baseflow") runoff </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.42685</span> </td>
#>    <td style="text-align:left;"> 8.35308 </td>
#>    <td style="text-align:left;"> 14.49035 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">73.5</span> </td>
#>    <td style="text-align:right;"> 1.59199 </td>
#>    <td style="text-align:right;"> 2.85029 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">79</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00025</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.07206</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 16 </td>
#>    <td style="text-align:left;"> Last date of a spring flood </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.76389</span> </td>
#>    <td style="text-align:left;"> 07-May </td>
#>    <td style="text-align:left;"> 23-Apr </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-14</span> </td>
#>    <td style="text-align:right;"> 4.00000 </td>
#>    <td style="text-align:right;"> 13.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">225</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02772</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.06656</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00111</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02836</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 17 </td>
#>    <td style="text-align:left;"> Duration of a spring flood </td>
#>    <td style="text-align:right;"> 1972 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.27922</span> </td>
#>    <td style="text-align:left;"> 41.71429 </td>
#>    <td style="text-align:left;"> 37.38889 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-10.4</span> </td>
#>    <td style="text-align:right;"> 6.26403 </td>
#>    <td style="text-align:right;"> 8.44455 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">34.8</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.25125</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.3627</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.18251</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.47353</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 18 </td>
#>    <td style="text-align:left;"> Maximum spring flood runoff </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">1.59615</span> </td>
#>    <td style="text-align:left;"> 375.6 </td>
#>    <td style="text-align:left;"> 280.1 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-25.4</span> </td>
#>    <td style="text-align:right;"> 223.47886 </td>
#>    <td style="text-align:right;"> 145.29060 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-35</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.7791</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.79459</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.40614</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.1788</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 19 </td>
#>    <td style="text-align:left;"> Date of a maximum spring flood runoff </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.45577</span> </td>
#>    <td style="text-align:left;"> 13-Apr </td>
#>    <td style="text-align:left;"> 09-Apr </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-4</span> </td>
#>    <td style="text-align:right;"> 6.00000 </td>
#>    <td style="text-align:right;"> 21.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">250</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.14618</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.73694</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.50793</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.03071</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 20 </td>
#>    <td style="text-align:left;"> Spring flood runoff volume (with groundwater and rain) </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.00044</span> </td>
#>    <td style="text-align:left;"> 0.30017 </td>
#>    <td style="text-align:left;"> 0.28816 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-4</span> </td>
#>    <td style="text-align:right;"> 0.15509 </td>
#>    <td style="text-align:right;"> 0.15447 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-0.4</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.83347</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.88179</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.85585</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 21 </td>
#>    <td style="text-align:left;"> Minimum daily winter runoff </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.36838</span> </td>
#>    <td style="text-align:left;"> 3.11364 </td>
#>    <td style="text-align:left;"> 8.14643 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">161.6</span> </td>
#>    <td style="text-align:right;"> 1.05163 </td>
#>    <td style="text-align:right;"> 2.20960 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">110.1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00025</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02415</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 22 </td>
#>    <td style="text-align:left;"> Date of minimum daily winter runoff </td>
#>    <td style="text-align:right;"> 1979 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">1.27273</span> </td>
#>    <td style="text-align:left;"> 08-Jan </td>
#>    <td style="text-align:left;"> 28-Jan </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">20</span> </td>
#>    <td style="text-align:right;"> 29.00000 </td>
#>    <td style="text-align:right;"> 42.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">44.8</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.31499</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.65457</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.18918</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.24899</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 23 </td>
#>    <td style="text-align:left;"> Minimum daily summer runoff </td>
#>    <td style="text-align:right;"> 1979 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.30608</span> </td>
#>    <td style="text-align:left;"> 3.43308 </td>
#>    <td style="text-align:left;"> 8.765 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">155.3</span> </td>
#>    <td style="text-align:right;"> 2.03321 </td>
#>    <td style="text-align:right;"> 1.66361 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-18.2</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">5e-05</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00049</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.51414</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 24 </td>
#>    <td style="text-align:left;"> Date of minimum daily summer runoff </td>
#>    <td style="text-align:right;"> 1971 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-2.22222</span> </td>
#>    <td style="text-align:left;"> 12-Aug </td>
#>    <td style="text-align:left;"> 14-Jul </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-29</span> </td>
#>    <td style="text-align:right;"> 31.00000 </td>
#>    <td style="text-align:right;"> 42.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">35.5</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.07967</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.42027</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.09794</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.52317</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 25 </td>
#>    <td style="text-align:left;"> Minimum 30-day averaged winter runoff </td>
#>    <td style="text-align:right;"> 1978 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.34624</span> </td>
#>    <td style="text-align:left;"> 5.09472 </td>
#>    <td style="text-align:left;"> 10.56721 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">107.4</span> </td>
#>    <td style="text-align:right;"> 1.31400 </td>
#>    <td style="text-align:right;"> 2.14514 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">63.3</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00031</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.11514</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 26 </td>
#>    <td style="text-align:left;"> First date of minimum 30-day averaged winter runoff </td>
#>    <td style="text-align:right;"> 1987 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.1334</span> </td>
#>    <td style="text-align:left;"> 01-Jan </td>
#>    <td style="text-align:left;"> 12-Dec </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">346</span> </td>
#>    <td style="text-align:right;"> 19.00000 </td>
#>    <td style="text-align:right;"> 28.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">47.4</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.83325</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.62829</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.26214</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.23087</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 27 </td>
#>    <td style="text-align:left;"> Minimum 30-day averaged summer runoff </td>
#>    <td style="text-align:right;"> 1976 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.36484</span> </td>
#>    <td style="text-align:left;"> 5.22917 </td>
#>    <td style="text-align:left;"> 10.19961 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">95.1</span> </td>
#>    <td style="text-align:right;"> 1.04442 </td>
#>    <td style="text-align:right;"> 2.52163 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">141.4</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00076</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.0117</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 28 </td>
#>    <td style="text-align:left;"> First date of minimum 30-day averaged summer runoff </td>
#>    <td style="text-align:right;"> 1972 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.97368</span> </td>
#>    <td style="text-align:left;"> 31-Jul </td>
#>    <td style="text-align:left;"> 09-Jul </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-22</span> </td>
#>    <td style="text-align:right;"> 24.00000 </td>
#>    <td style="text-align:right;"> 28.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">16.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.26127</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.68145</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.06812</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.7119</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 29 </td>
#>    <td style="text-align:left;"> Minimum 10-day averaged winter runoff </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.34266</span> </td>
#>    <td style="text-align:left;"> 4.11427 </td>
#>    <td style="text-align:left;"> 9.03429 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">119.6</span> </td>
#>    <td style="text-align:right;"> 0.99713 </td>
#>    <td style="text-align:right;"> 2.13112 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">113.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00025</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02134</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 30 </td>
#>    <td style="text-align:left;"> First date of minimum 10-day averaged winter runoff </td>
#>    <td style="text-align:right;"> 1987 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.43922</span> </td>
#>    <td style="text-align:left;"> 24-Jan </td>
#>    <td style="text-align:left;"> 01-Jan </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-23</span> </td>
#>    <td style="text-align:right;"> 31.00000 </td>
#>    <td style="text-align:right;"> 30.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-3.2</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.60689</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.97855</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.2174</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.8905</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 31 </td>
#>    <td style="text-align:left;"> Minimum 10-day averaged summer runoff </td>
#>    <td style="text-align:right;"> 1979 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.30702</span> </td>
#>    <td style="text-align:left;"> 5.40946 </td>
#>    <td style="text-align:left;"> 9.35483 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">72.9</span> </td>
#>    <td style="text-align:right;"> 1.33854 </td>
#>    <td style="text-align:right;"> 1.96794 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">47</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00095</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">1e-05</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.20118</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 32 </td>
#>    <td style="text-align:left;"> First date of minimum 10-day averaged summer runoff </td>
#>    <td style="text-align:right;"> 1972 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-1.15476</span> </td>
#>    <td style="text-align:left;"> 06-Aug </td>
#>    <td style="text-align:left;"> 14-Jul </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-23</span> </td>
#>    <td style="text-align:right;"> 31.00000 </td>
#>    <td style="text-align:right;"> 29.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-6.5</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.19839</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.57756</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.12978</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.7422</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 33 </td>
#>    <td style="text-align:left;"> Minimum 5-day averaged winter runoff </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.35914</span> </td>
#>    <td style="text-align:left;"> 3.71236 </td>
#>    <td style="text-align:left;"> 8.68714 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">134</span> </td>
#>    <td style="text-align:right;"> 0.95463 </td>
#>    <td style="text-align:right;"> 2.20355 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">130.8</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00025</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.01201</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 34 </td>
#>    <td style="text-align:left;"> First date of minimum 5-day averaged winter runoff </td>
#>    <td style="text-align:right;"> 1987 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.03125</span> </td>
#>    <td style="text-align:left;"> 25-Jan </td>
#>    <td style="text-align:left;"> 30-Dec </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">340</span> </td>
#>    <td style="text-align:right;"> 34.00000 </td>
#>    <td style="text-align:right;"> 33.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-2.9</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.97855</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.21728</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.8659</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 35 </td>
#>    <td style="text-align:left;"> Minimum 5-day averaged summer runoff </td>
#>    <td style="text-align:right;"> 1979 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.29795</span> </td>
#>    <td style="text-align:left;"> 5.26431 </td>
#>    <td style="text-align:left;"> 9.11483 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">73.1</span> </td>
#>    <td style="text-align:right;"> 1.30323 </td>
#>    <td style="text-align:right;"> 1.91290 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">46.8</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00095</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">1e-05</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.20305</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 36 </td>
#>    <td style="text-align:left;"> First date of minimum 5-day averaged summer runoff </td>
#>    <td style="text-align:right;"> 1972 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-1.33158</span> </td>
#>    <td style="text-align:left;"> 09-Aug </td>
#>    <td style="text-align:left;"> 19-Jul </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-21</span> </td>
#>    <td style="text-align:right;"> 30.00000 </td>
#>    <td style="text-align:right;"> 29.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-3.3</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.16078</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.57756</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.13212</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.84737</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 37 </td>
#>    <td style="text-align:left;"> Maximum thaw flood runoff </td>
#>    <td style="text-align:right;"> 1984 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.04077</span> </td>
#>    <td style="text-align:left;"> 23.8893 </td>
#>    <td style="text-align:left;"> 15.30978 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-35.9</span> </td>
#>    <td style="text-align:right;"> 28.87597 </td>
#>    <td style="text-align:right;"> 27.90029 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-3.4</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.90704</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.48376</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.50797</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.99274</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 38 </td>
#>    <td style="text-align:left;"> Date of a maximum thaw flood runoff </td>
#>    <td style="text-align:right;"> 1975 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">0</span> </td>
#>    <td style="text-align:left;"> 02-Jan </td>
#>    <td style="text-align:left;"> 22-Dec </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">355</span> </td>
#>    <td style="text-align:right;"> 39.00000 </td>
#>    <td style="text-align:right;"> 38.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(211, 211, 211, 1) !important;">-2.6</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.96269</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">1</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.52561</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.86402</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 39 </td>
#>    <td style="text-align:left;"> Number of thaw flood events </td>
#>    <td style="text-align:right;"> 1979 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.1</span> </td>
#>    <td style="text-align:left;"> 12.91667 </td>
#>    <td style="text-align:left;"> 14.54545 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">12.6</span> </td>
#>    <td style="text-align:right;"> 4.66044 </td>
#>    <td style="text-align:right;"> 3.55988 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-23.6</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.52327</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.97466</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.35504</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.40482</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 40 </td>
#>    <td style="text-align:left;"> Number of thaw flood days </td>
#>    <td style="text-align:right;"> 1984 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-2</span> </td>
#>    <td style="text-align:left;"> 82.55556 </td>
#>    <td style="text-align:left;"> 45.57143 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-44.8</span> </td>
#>    <td style="text-align:right;"> 19.51537 </td>
#>    <td style="text-align:right;"> 17.09637 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-12.4</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00435</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.01648</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00049</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.78874</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 41 </td>
#>    <td style="text-align:left;"> Maximum rain flood runoff </td>
#>    <td style="text-align:right;"> 1978 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">3.92943</span> </td>
#>    <td style="text-align:left;"> 41.45077 </td>
#>    <td style="text-align:left;"> 93.00458 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">124.4</span> </td>
#>    <td style="text-align:right;"> 33.96865 </td>
#>    <td style="text-align:right;"> 39.40497 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">16</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00029</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.01948</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00189</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.63011</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 42 </td>
#>    <td style="text-align:left;"> Date of a maximum rain flood runoff </td>
#>    <td style="text-align:right;"> 1980 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.81176</span> </td>
#>    <td style="text-align:left;"> 08-Jul </td>
#>    <td style="text-align:left;"> 31-May </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-38</span> </td>
#>    <td style="text-align:right;"> 80.00000 </td>
#>    <td style="text-align:right;"> 64.00000 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-20</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.36212</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.32756</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.20321</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.48496</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 43 </td>
#>    <td style="text-align:left;"> Number of rain flood events </td>
#>    <td style="text-align:right;"> 1977 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.29286</span> </td>
#>    <td style="text-align:left;"> 20.36364 </td>
#>    <td style="text-align:left;"> 15.07143 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-26</span> </td>
#>    <td style="text-align:right;"> 4.58852 </td>
#>    <td style="text-align:right;"> 3.07507 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-33</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00906</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02918</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00442</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.17754</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 44 </td>
#>    <td style="text-align:left;"> Number of rain flood days </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">1.6015</span> </td>
#>    <td style="text-align:left;"> 89.4 </td>
#>    <td style="text-align:left;"> 123.25 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">37.9</span> </td>
#>    <td style="text-align:right;"> 9.93982 </td>
#>    <td style="text-align:right;"> 30.16250 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">203.5</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.05231</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.08206</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00043</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.0439</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 45 </td>
#>    <td style="text-align:left;"> Relative variation of runoff during winter low flow </td>
#>    <td style="text-align:right;"> 1983 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-0.00906</span> </td>
#>    <td style="text-align:left;"> 0.40749 </td>
#>    <td style="text-align:left;"> 0.24237 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-40.5</span> </td>
#>    <td style="text-align:right;"> 0.26845 </td>
#>    <td style="text-align:right;"> 0.22980 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-14.4</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.07212</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.06656</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.13233</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.70453</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 46 </td>
#>    <td style="text-align:left;"> Duration of winter low flow </td>
#>    <td style="text-align:right;"> 1984 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-1.09545</span> </td>
#>    <td style="text-align:left;"> 127.05556 </td>
#>    <td style="text-align:left;"> 109.28571 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-14</span> </td>
#>    <td style="text-align:right;"> 19.69315 </td>
#>    <td style="text-align:right;"> 17.77371 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(173, 216, 230, 1) !important;">-9.7</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.10688</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.34481</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.05002</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.85348</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 47 </td>
#>    <td style="text-align:left;"> Relative variation of runoff during summer-autumn low flow </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">0.02475</span> </td>
#>    <td style="text-align:left;"> 0.27842 </td>
#>    <td style="text-align:left;"> 0.70136 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">151.9</span> </td>
#>    <td style="text-align:right;"> 0.09683 </td>
#>    <td style="text-align:right;"> 0.29290 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">202.5</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.02077</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.07658</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">3e-05</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(230, 230, 0, 1) !important;">0.04442</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:right;"> 48 </td>
#>    <td style="text-align:left;"> Duration of a summer-autumn low flow </td>
#>    <td style="text-align:right;"> 1970 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">1</span> </td>
#>    <td style="text-align:left;"> 184.4 </td>
#>    <td style="text-align:left;"> 206.4 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">11.9</span> </td>
#>    <td style="text-align:right;"> 10.16366 </td>
#>    <td style="text-align:right;"> 23.49558 </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 192, 203, 1) !important;">131.2</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.08346</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.21156</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(153, 204, 0, 1) !important;">0.00603</span> </td>
#>    <td style="text-align:left;"> <span style="border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 153, 102, 1) !important;">0.11559</span> </td>
#>   </tr>
#> </tbody>
#> </table>
```
