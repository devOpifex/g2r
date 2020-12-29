
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

# g2r

[G2.js](https://g2.antv.vision/en) for R.

## Installation

You can install package from Github

``` r
# install.packages("remotes")
remotes::install_github("devOpifex/g2r")
```

## Example

``` r
library(g2r)

g2(cars, asp(speed, dist)) %>% 
  fig_point()
```
