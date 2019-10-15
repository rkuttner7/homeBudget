
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homeBudget

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Travis build
status](https://travis-ci.org/rkuttner7/homeBudget.svg?branch=master)](https://travis-ci.org/rkuttner7/homeBudget)
<!-- badges: end -->

The goal of homeBudget is to make monitoring expenses easy. Keep a
record of all spending in one place in a consistent form with custom
categories.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rkuttner7/homeBudget")
```

Windows users also must first install
[Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

## Resources

#### Vignette

For details on uploading csv statements as well as saving and loading
your data see the `getting-started` vignette.

``` r
vignette("getting-started", package = "homeBudget")
```

<br>

#### Report Template

Here is an example of using `homeBudget` to create an [expense
report](https://rawcdn.githack.com/rkuttner7/homeBudget/de8a47f9feffa02b3128190e6ae4b3cd1bf12e41/inst/rmd/budgetReport.html).
Feel free to use this as a template for your own budgeting. The code can
be found here:

``` r
system.file(package = "homeBudget", "rmd", "budgetReport.Rmd")
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/rkuttner7/homeBudget/issues).

## <br>
