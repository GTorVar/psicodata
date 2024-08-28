
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psicodata

<!-- badges: start -->
<!-- badges: end -->

The goal of psicodata is to make easier usual analysis in psychology,
oriented to generate APA format tables. This package returns values in
Spanish as usual form.

## Installation

You can install the development version of psicodata from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("GTorVar/psicodata")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(psicodata)
## basic example code
x <- data.frame(A = c(1,1,2,3), B=c(2,2,3,4))
moda(x)
#> $result
#>      A B
#> Moda 1 2
#> 
#> $warning
#> character(0)
```
