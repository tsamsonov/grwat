[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/gpl-2.0.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-yellow.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Coverage Status](https://img.shields.io/codecov/c/github/tsamsonov/grwat/main.svg)](https://codecov.io/github/tsamsonov/grwat?branch=main)
[![R-CMD-check](https://github.com/tsamsonov/grwat/workflows/R-CMD-check/badge.svg)](https://github.com/tsamsonov/grwat/actions)

# grwat

Welcome to grwat, an R package package for the automatic hydrograph separation and hydrological time series analysis. High-performance `C++17` computation is used for hydrograph processing that separates runoff into ground, seasonal, thaw, and flood runoff. Interannual and long-term characteristics of each runoff type are derived. Results are visualized in a form of high-quality reports making use of ggplot2 graphics and knitr report generation.

## Installing

__grwat__ is not available on CRAN yet. You can install it from GitHub repository. For this three steps are required:

1. Install devtools R package
2. Install compiler (Windows and macOS only)
3. Install grwat R package

### Install devtools 

To install from GitHub, you should install `devtools` package first (unless it is already installed on your machine):

```r
install.packages("devtools")
```

### Install compiler

Since grwat contains C++ code, it needs to be compiled during the package installation. 

__Linux__ users should have the compiler already installed in their system. 

__macOS__ users have to:

1. Install [Xcode command-line tools](https://developer.apple.com/download/more/).
2. Restart R session.

__Windows__ users have to:

1. Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
2. Restart R session.

### Install grwat

If all previous steps are completed successfully, grwat package can be installed via single command:
```r
devtools::install_github("tsamsonov/grwat")
```

> __A note to Windows users:__ if you get the error during installation _over the previously installed grwat_, remove the package folder manually, restart R and then hit `devtools::install_github("tsamsonov/grwat", INSTALL_opts = '--no-lock')`. You should run RStudio as Administrator to get the full access to the package installation folder. The location of installation folder can be learned from _Packages — Install_ dialog or by `.libPaths()` command in R console as displayed below.

```
> .libPaths()
[1] "C:/Users/tsamsonov/Documents/R/win-library/4.1"
[2] "C:/Program Files/R/R-4.1.0/library" 
```

## Funding

__grwat__ package is being developed with financial support of Russian Science Foundation (Project [19-77-10032](https://rscf.ru/upload/iblock/329/3294f294b9a3a424e3044797a0e6bd6f.pdf)).

The main separation algoritm was developed in 2016-2018 with financial support of RFBR (Project [16-35-60080](https://www.rfbr.ru/rffi/ru/project_search/o_2031785)).

The mountain block of the main separation algoritm was developed in 2018-2019 with financial support of Russian Science Foundation (Project [17-77-10169](https://rscf.ru/sites/default/files/docfiles/ONG_2017.pdf)).
