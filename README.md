[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg?style=flat)](https://opensource.org/licenses/MIT/)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-yellow.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Coverage Status](https://img.shields.io/codecov/c/github/tsamsonov/grwat/main.svg)](https://app.codecov.io/github/tsamsonov/grwat?branch=main)
[![R-CMD-check](https://github.com/tsamsonov/grwat/workflows/R-CMD-check/badge.svg)](https://github.com/tsamsonov/grwat/actions/)
[![r-universe](https://tsamsonov.r-universe.dev/badges/grwat)](https://tsamsonov.r-universe.dev/ui#package:grwat)
[![CRAN](http://www.r-pkg.org/badges/version/grwat)](https://cran.r-project.org/package=grwat)
[![CRAN checks](https://badges.cranchecks.info/worst/grwat.svg](https://cran.r-project.org/web/checks/check_results_grwat.html)
[![Monthly downloads](http://cranlogs.r-pkg.org/badges/grwat?color=brightgreen)](http://www.r-pkg.org/pkg/grwat)
[![Total downloads](http://cranlogs.r-pkg.org/badges/grand-total/grwat)](https://cran.r-project.org/package=grwat)

# grwat

<img src="https://raw.githubusercontent.com/tsamsonov/grwat/master/vignettes/grwat_logo.svg" align="right" alt="" width="150" />

Welcome to __`grwat`__, an R package for the automatic hydrograph separation and daily hydrological time series analysis. __`grwat`__ provides various filters to separate baseflow and quickflow. Implements advanced separation technique which involves meteorological data to reveal genetic components of the runoff: ground, rain, thaw and spring (seasonal thaw). High-performance `C++17` computation, annually aggregated variables, statistical testing and numerous [ggplot](https://ggplot2.tidyverse.org)-based functions for informative plotting.

> __Important note:__ The current state of the package should be considered _experimental_. Convenience of __`grwat`__ data model and processing workflow should be tested by package users and may change in near future. Feel free to submit bugs and suggestions on improvement of the package to the [GitHub issues](https://github.com/tsamsonov/grwat/issues).

## Install released version from CRAN

Install the latest released version of __`grwat`__ from CRAN by:

```r
install.packages("grwat")
```

## Install development version from R-universe

The current development version of __`grwat`__ can be installed from [R-universe](https://tsamsonov.r-universe.dev/ui#package:grwat):

```r
# Enable repository from tsamsonov
options(
  repos = c(
    ropensci = 'https://tsamsonov.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'
  )
)
  
# Download and install grwat in R
install.packages('grwat')
```

## Install development version from GitHub

The current development version of __`grwat`__ can be installed from [GitHub](https://github.com/tsamsonov/grwat/). For this three steps are required:

1. Install __`remotes`__ R package
2. Install compiler (Windows and macOS only)
3. Install __`grwat`__ R package

### Install remotes 

To install from GitHub, you should install __`remotes`__ package first (unless it is already installed on your machine):

```r
install.packages("remotes")
```

### Install compiler

Since __`grwat`__ contains C++ code, it needs to be compiled during the package installation. 

__Linux__ users should have the compiler already installed in their system. 

__macOS__ users have to:

1. Install [Xcode command-line tools](https://developer.apple.com/xcode/resources/).
2. Restart R session.

__Windows__ users have to:

1. Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
2. Restart R session.

### Install grwat

If all previous steps are completed successfully, __`grwat`__ package can be installed via single command:
```r
remotes::install_github("tsamsonov/grwat")
```

> __A note to Windows users:__ if you get the error during installation _over the previously installed grwat_, remove the package folder manually, restart R and then hit `remotes::install_github("tsamsonov/grwat", INSTALL_opts = '--no-lock')`. You should run RStudio as Administrator to get the full access to the package installation folder. The location of installation folder can be learned from _Packages — Install_ dialog or by `.libPaths()` command in R console as displayed below.

```
> .libPaths()
[1] "C:/Users/tsamsonov/Documents/R/win-library/4.1"
[2] "C:/Program Files/R/R-4.1.0/library" 
```

## Why 'grwat'?

___grwat___ is an acronym made from <b><i>gr</i></b>ound <b><i>wat</i></b>er. This name emerged historically because the extraction of the ground flow (baseflow) is one of the most important stages in the advanced separation algorithm provided by the package.

## Funding

__`grwat`__ package has been developed in 2019-2022 with financial support of Russian Science Foundation (RSF) Project [19-77-10032](https://rscf.ru/upload/iblock/329/3294f294b9a3a424e3044797a0e6bd6f.pdf).

The main separation algorithm was developed in 2016-2018 with financial support of Russian Foundation for Basic Research (RFBR) Project [16-35-60080](https://www.rfbr.ru/rffi/ru/project_search/o_2031785).

The mountain block of the main separation algorithm was developed in 2018-2019 with financial support of Russian Science Foundation (RSF) Project [17-77-10169](https://rscf.ru/sites/default/files/docfiles/ONG_2017.pdf).
