
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RLR

The goal of RLR is to help you conduct proper RL research.

## Installation

You can install the development version of RLR from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("shahar-lab/RLR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(RLR)
## basic example code
R=
randomwalk(Narms=4,
          Ntrials=50,
           tau            =.02,#standard deviation for the noise normal distribution of each arm
           rho            =0, #true correlation between all arms. default should be zero
           upper.bound   =0.85,
           lower.bound   =0.15)
```
