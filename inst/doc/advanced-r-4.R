## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

## ---- include = FALSE----------------------------------------------------
library(ggplot2)
library(linear.regression)

## ----echo=TRUE, fig.show='hold'------------------------------------------
linreg_mod = linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

## ---- fig.show='hold'----------------------------------------------------
linreg_mod$print()

## ------------------------------------------------------------------------
linreg_mod$plot()

## ---- fig.show='hold', fig.align='center'--------------------------------
head(linreg_mod$resid())

## ---- fig.show='hold'----------------------------------------------------
head(linreg_mod$pred())

## ---- fig.show='hold'----------------------------------------------------
linreg_mod$coef()

## ---- fig.show='hold'----------------------------------------------------
linreg_mod$summary()

