# Advanced Programming in R - Assignment 4
[![Build Status](https://travis-ci.org/AnnalenaE/advanced-r-4.svg?branch=master)](https://travis-ci.org/AnnalenaE/advanced-r-4)
[![Coverage status](https://codecov.io/gh/AnnalenaE/advanced-r-4/branch/master/graph/badge.svg)](https://codecov.io/github/AnnalenaE/advanced-r-4?branch=master)

This is the 4rd assignment of the course Advanced Programming in R at Linköping University in 2018.

Course information and all assignments can be found at https://www.ida.liu.se/~732A94/info/courseinfo.en.shtml.

## Exercise
The exercise for the 4rd assignment is to implement a linear regression and create some plots.

## Installation

The package can be downloaded from GitHub via R with:

```{r installation, eval = FALSE}
devtools::install_github("AnnalenaE/advanced-r-4")
```

## Vignette

After installing, run:

```{r installation, eval = FALSE}
browseVignettes("linear.regression")
```

## Examples

```{r installation, eval = FALSE}
linreg_mod = linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_mod$print()

#> Coefficients:

#>  (Intercept) Sepal.Width Sepal.Length
#>       -2.525      -1.339        1.776

#> Call:
#> linreg(formula = Petal.Length ~ Sepal.Width + Sepal.Length, data = iris)

linreg_mod$summary()

#> Coefficients:

#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)     -2.52       0.56   -4.48 1.48e-05 ***
#> Sepal.Width     -1.34       0.12  -10.94 9.43e-21 ***
#> Sepal.Length     1.78       0.06   27.57 5.85e-60 ***

#> Residual standard error: 0.64648051265712 on 147 degrees of freedom
```
