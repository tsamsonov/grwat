[![License](http://img.shields.io/badge/license-GPL%20%28%3E=%202%29-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-2.0.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# grwat

Welcome to grwat, an R package for hydrograph separation and analysis based on water level, temperature and percipitation data. It makes use of geographic data processing to spatially select temperature and precipitation data within the basin of each gauge, average these data and join them to each element in water level series. High-preformance Fortran/C++ computation is used for hydrograph processing that separates water level series into ground, seasonal, thaw, and flood discharge. Interannual and long-term characteristics of each discharge type are derived. Results are visualized in a form of high-quality reports making use of ggplot2 graphics and knitr report generation

## Installing

### Install devtools 

Currently the package is available from GitHub repository. To install from GitHub, you should install `devtools` package first (unless it is already installed on your machine):

```r
install.packages("devtools")
```

### Install grwat

#### Binary (R 4.1 only) 

If you have R 4.1 installed, then the easiest way to install on Windows and macOS is to use the compiled binary package. 

Windows:
```r
devtools::install_url('https://tsamsonov.github.io/grwat/build/grwat.zip', 
                      build = FALSE, dependencies = TRUE)
```

macOS:
```r
devtools::install_url('https://tsamsonov.github.io/grwat/build/grwat.tgz', 
                      build = FALSE, dependencies = TRUE)
```

#### From sources

Since grwat contains C++ code, it needs to be compiled during the package installation. 

__Linux__ users should have the compiler already installed in their system. 

__macOS__ users have to:

1. Install [Xcode command-line tools](https://developer.apple.com/download/more/).
2. Restart R session.

__Windows__ users have to:

1. Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
2. Restart R session.

If all previous steps are completed successfully, grwat package can be installed via single command:
```r
devtools::install_github("tsamsonov/grwat")
```

## Funding

__grwat__ package is being developed with financial support of Russian Science Foundation (Project [19-77-10032](https://rscf.ru/upload/iblock/329/3294f294b9a3a424e3044797a0e6bd6f.pdf)).

The main separation algoritm was developed in 2016-2018 with financial support of RFBR (Project [16-35-60080](http://www.rfbr.ru/rffi/ru/project_search/o_2031785)).

The mountain block of the main separation algoritm was developed in 2018-2019 with financial support of Russian Science Foundation (Project [17-77-10169](http://rscf.ru/sites/default/files/docfiles/ONG_2017.pdf)).
