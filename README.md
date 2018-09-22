# Advanced Programming in R - Assignment 4
[![Build Status](https://travis-ci.org/AnnalenaE/advanced-r-4.svg?branch=master)](https://travis-ci.org/AnnalenaE/advanced-r-4)
[![Coverage status](https://codecov.io/gh/AnnalenaE/advanced-r-4/branch/master/graph/badge.svg)](https://codecov.io/github/AnnalenaE/advanced-r-4?branch=master)

## ToDo
- ~~Code cleanup!~~
- ~~Make `summary()` and `print()` pass unit tests~~
  - ~~They now use print(data.frame) for display, which is much nicer. Still they don't pass the unit tests.~~
- ~~Implement `plot()` function~~
- ~~Documentation of the RC methods~~
- ~~`Pr(>|t|)` in `summary()` is missing~~
- `l_t_beta` is calculated in the constructor. Later on in the `summary()` function this matrix is not used, but instead `local_t_value = l_beta[i]/sqrt(l_var_beta[i, i])` is used to calculate the values. When trying to use the calculated values, the values are wrong.
- Write Vignette & update this `README.md`
- ~~Code documentation~~
- ~~Add TravisCI, badge and try to import package (also fill this `README.md`)~~
- (optional) QR-Decomposition
- (optional) Add a theme
