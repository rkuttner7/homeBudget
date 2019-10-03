
<!-- README.md is generated from README.Rmd. Please edit that file -->
homeBudget
==========

<!-- badges: start -->
<!-- badges: end -->
The goal of homeBudget is to make monitoring expenses easy. Keep a record of all spending in one place in a consistent form with custom categories.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rkuttner7/homeBudget")
```

Windows users also must first install [Rtools](http://cran.rstudio.com/bin/windows/Rtools/).

Resources
---------

#### Vingette

For details on uploading csv statements as well as saving and loading your data see the `getting-started` vingette.

``` r
vignette("getting-started", package = "homeBudget")
```

<br>

#### Report Template

Here is an example of using `homeBudget` to create an [expense report](https://github.com/rkuttner7/homeBudget/blob/master/inst/rmd/budgetReport.html). Feel free to use this as a template for your own budgeting. The code can be found here:

``` r
system.file(package = "homeBudget", "rmd", "budgetReport.Rmd")
```
