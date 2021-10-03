
<div align="center">

<img src="man/figures/logo.png" height = "200px" />

<br />
<br />

<!-- badges: start -->

[![R-CMD-check](https://github.com/devOpifex/g2r/workflows/R-CMD-check/badge.svg)](https://github.com/devOpifex/g2r/actions)
[![Coveralls test coverage](https://coveralls.io/repos/github/devOpifex/g2r/badge.svg)](https://coveralls.io/github/devOpifex/g2r)
<!-- badges: end -->

[G2.js](https://g2.antv.vision/) for R.

[Website](https://g2r.opifex.org) | [Docs](https://g2r.opifex.org/articles/docs.html) | [Get
Started](https://g2r.opifex.org/articles/get-started.html)

</div>

## Installation

You can install package from Github.

```r
# install.packages("remotes")
remotes::install_github("devOpifex/g2r")
```

## Example

```r
library(g2r)

g2(cars, asp(speed, dist, color = dist)) %>% 
  fig_point(asp(size = dist)) %>% 
  gauge_color_viridis()
```

![](man/figures/g2-example.png)

## Limitations

g2r currently does not work well with _large datasets_ (performance drop), and some features are still under development, namely _rmarkdown actions_, _shiny proxies_, and _maps_. _Theming_ is not currently rather unwieldy.
