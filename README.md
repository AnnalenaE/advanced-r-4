# Advanced Programming in R - Assignment 4
[![Build Status](https://travis-ci.org/AnnalenaE/advanced-r-4.svg?branch=master)](https://travis-ci.org/AnnalenaE/advanced-r-4)
[![Coverage status](https://codecov.io/gh/AnnalenaE/advanced-r-4/branch/master/graph/badge.svg)](https://codecov.io/github/AnnalenaE/advanced-r-4?branch=master)

## Exercise

This package uses a linear regression model. It uses linear algebra to give a linear regresion model functionality when given a formula and a dataframe to work with. for the following examples the dataset `iris` will be used.

## Installation 

The package can be downloaded from GitHub via R with:

```{r installation, eval = FALSE}
devtools::install_github("AnnalenaE/advanced-r-4")
```

## Example Usage Methods


### print() 

A method call  that gives back the formula along with the coeficients.

### plot() 

This one method call returns two plots containning the residuals in relation to the fitted values. the first one gives the Residuals vs Fitted, while the seccond one gives the Scale - Location.

### resid()
A method to call on the residuals

$$\hat{e} = y - \hat{y} = y - X\hat{\beta}$$

### pred() 
A method call to get the predicted values $\hat{y}$.`

### coef() 

A method call to get the coefficients as a named vector.

### summary()
This returns a printout presenting the coefficients with their standard error, t-value and p-value as well as the estimate of $\hat{\sigma}$ along with the degrees of freedom in the model.

## References 

[Matrix decompositions for regression analysis](https://www.stat.wisc.edu/courses/st849-bates/lectures/Orthogonal.pdf)

 [Some	Notes	on	Least	Squares, QR-factorization, SVD and	Fitting](http://staff.www.ltu.se/~jove/courses/c0002m/least_squares.pdf)
