
<!-- README.md is generated from README.Rmd. Please edit that file -->

# multilandr <img src="man/figures/logo.png" align="right" alt="" width="150" />

<!-- badges: start -->
<!-- badges: end -->

The goal of **multilandr** is to provide a user-friendly platform for
typical landscape-scale analysis. The package builds on several
spatial-oriented R packages to provide a useful tool to develop and
inspect landscapes at multiple spatial scales. The main functionality
allows to calculate landscape metrics within a multi-scale approach.
Among other practical capabilities, the package provides several utility
functions to: (i) plot landscapes at different spatial scales; (ii)
visualize correlations between different metrics; (iii) filter
landscapes that fulfill with certain pre-defined conditions regarding
their metrics; and (i) generate optimized gradients for a given
landscape metric.

**multilandr** supports several spatial objects from widespread
spatial-oriented R packages as main inputs in their functions, including
objects from packages **terra**, **raster**, and **sf**.

## Installation

You can install the development version of multilandr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("phuais/multilandr")
```

## Using multilandr

This is a basic example which shows you how to solve a common problem:

``` r
library(multilandr)
## basic example code
```
