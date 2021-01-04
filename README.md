
<div align="center">

<img src="man/figures/logo.png" height = "200px" />

<br />
<br />

<!-- badges: start -->

<!-- badges: end -->

[G2.js](https://g2.antv.vision/) for R.

[Website](https://g2r.dev) | [Get
Started](https://g2r.dev/articles/get_started.html)

</div>

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
