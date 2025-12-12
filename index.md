# grwat

![](https://raw.githubusercontent.com/tsamsonov/grwat/master/vignettes/grwat_logo.svg)

Welcome to **`grwat`**, an R package for the automatic hydrograph
separation and daily hydrological time series analysis. **`grwat`**
provides various filters to separate baseflow and quickflow. Implements
advanced separation technique which involves meteorological data to
reveal genetic components of the runoff: ground, rain, thaw and spring
(seasonal thaw). High-performance `C++17` computation, annually
aggregated variables, statistical testing and numerous
[ggplot](https://ggplot2.tidyverse.org)-based functions for informative
plotting.

## Install released version from CRAN

Install the latest released version of **`grwat`** from CRAN by:

``` r
install.packages("grwat")
```

## Install development version from R-universe

The current development version of **`grwat`** can be installed from
[R-universe](https://tsamsonov.r-universe.dev/grwat):

``` r
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

The current development version of **`grwat`** can be installed from
[GitHub](https://github.com/tsamsonov/grwat/). For this three steps are
required:

1.  Install **`remotes`** R package
2.  Install compiler (Windows and macOS only)
3.  Install **`grwat`** R package

### Install remotes

To install from GitHub, you should install **`remotes`** package first
(unless it is already installed on your machine):

``` r
install.packages("remotes")
```

### Install compiler

Since **`grwat`** contains C++ code, it needs to be compiled during the
package installation.

**Linux** users should have the compiler already installed in their
system.

**macOS** users have to:

1.  Install [Xcode command-line
    tools](https://developer.apple.com/xcode/resources/).
2.  Restart R session.

**Windows** users have to:

1.  Install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
2.  Restart R session.

### Install grwat

If all previous steps are completed successfully, **`grwat`** package
can be installed via single command:

``` r
remotes::install_github("tsamsonov/grwat")
```

> **A note to Windows users:** if you get the error during installation
> *over the previously installed grwat*, remove the package folder
> manually, restart R and then hit
> `remotes::install_github("tsamsonov/grwat", INSTALL_opts = '--no-lock')`.
> You should run RStudio as Administrator to get the full access to the
> package installation folder. The location of installation folder can
> be learned from *Packages — Install* dialog or by
> [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) command in R
> console as displayed below.

    > .libPaths()
    [1] "C:/Users/tsamsonov/Documents/R/win-library/4.1"
    [2] "C:/Program Files/R/R-4.1.0/library" 

## Why ‘grwat’?

***grwat*** is an acronym made from ***gr***ound ***wat***er. This name
emerged historically because the extraction of the ground flow
(baseflow) is one of the most important stages in the advanced
separation algorithm provided by the package.

## Funding

**`grwat`** package has been developed in 2019-2022 with financial
support of Russian Science Foundation (RSF) Project 19-77-10032.

The main separation algorithm was developed in 2016-2018 with financial
support of Russian Foundation for Basic Research (RFBR) Project
16-35-60080.

The mountain block of the main separation algorithm was developed in
2018-2019 with financial support of Russian Science Foundation (RSF)
Project 17-77-10169.
