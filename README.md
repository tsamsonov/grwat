[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)



# grwat

Welcome to grwat, an R package for hydrograph separation and analysis based on water level, temperature and percipitation data. It makes use of geographic data processing to spatially select temperature and precipitation data within the basin of each gauge, average these data and join them to each element in water level series. High-preformance Fortran/C++ computation is used for hydrograph processing that separates water level series into ground, seasonal, thaw, and flood discharge. Interannual and long-term characteristics of each discharge type are derived. Results are visualized in a form of high-quality reports making use of ggplot2 graphics and knitr report generation

## Installing

To use grwat on your machine you need to install:

1. devtools package
2. command-line development tools _(Windows and macOS users only)_
3. grwat package itself
4. TeX distribution _(only if you need PDF reporting)_

### Install devtools package

[__devtools__](https://cran.r-project.org/web/packages/devtools/index.html) is a great library that facilitates working with in-development packages not hosted on CRAN. You have to install it first (unless it is already installed on your machine):
```r
install.packages("devtools")
```

### Install command-line development tools

Since grwat contains C++ code, it needs to be compiled during the package installation. 

__Linux__ users should have the compiler already installed in their system. 

__macOS__ users have to:

1. Install [Xcode command-line tools](https://developer.apple.com/download/more/).
2. Restart R session.

__Windows__ users have to:

1. Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
2. Restart R session.
3. Run the following code in R command line:

```r
library(devtools)
assignInNamespace("version_info", c(devtools:::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
```

### Install grwat package

If all previous steps are completed successfully, grwat package can be installed via single command:
```r
devtools::install_github("tsamsonov/grwat")
```

### Install TeX distribution

One of the key features of __grwat__ is reporting. Reports are generated in HTML by default. PDF reporting is also supported, but this requires installed TeX distribution. 

> TeX is not required if you use report generation in HTML format

The easiest way to get TeX is to use [__tinytex__](https://yihui.name/tinytex/) package. It provides a small TeX distribution that installs LaTeX packages automatically as soon as they are needed.

The following commands will install tinytex package and TeX distribution underlying it:
```r
install.packages('tinytex')
tinytex::install_tinytex()
```

Then restart R session and run `tinytex:::is_tinytex()`. If everything is OK, you will get `TRUE` as a result:
```r
tinytex:::is_tinytex()
[1] TRUE
```

> Currently only English locale is supported in PDF reports. Russian locale is unavailable due to TeX limitaions

## Using

Check out the __Get started__ vignette at the top of the page.

## Acknowledgment

The main separation algoritm grwat package was developed with financial support of RFBR (Project [16-35-60080](http://www.rfbr.ru/rffi/ru/project_search/o_2031785)).

The mountain block of the program was created with support of Russian Scientific Foundation (Project [17-77-10169](http://rscf.ru/sites/default/files/docfiles/ONG_2017.pdf)).
