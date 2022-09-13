
<!-- README.md is generated from README.Rmd. -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/lbau7/regtabstar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lbau7/regtabstar/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# regtabstar

`regtabstar` creates nice looking regression tables for statistical
reports written in LaTeX, HTML or Word.

## Installation

To install the package write:

``` r
# install.packages("devtools")
devtools::install_github("lbau7/textools")
```

## Usage

The main function of `regtabstar` is `regtab()` which creates tables for
the regression output of various models.

``` r
library(regtabstar)

iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Species, data = iris)
regtab(iris.lm, format = "pipe")
```

|                   | Estimate | Lower CL | Upper CL | p-Value |
|:------------------|:---------|:---------|:---------|:--------|
| Sepal.Width       | 0.698    | 0.462    | 0.934    | \<0.001 |
| Petal.Width       | 0.372    | -0.02    | 0.764    | 0.063   |
| setosa            | 0        | .        | .        | .       |
| Speciesversicolor | 0.988    | 0.445    | 1.531    | \<0.001 |
| Speciesvirginica  | 1.238    | 0.464    | 2.011    | 0.002   |

Linear Model (n = 150)
