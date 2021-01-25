<br/>

<div class="row">

<div class="jumbotron">
<img src="reference/figures/logo.png" height="200px" align="right"/>
<h4>An R wrapper for the <a href="https://g2.antv.vision/en" class="link" target="_blank">G2.js</a> visualisation library.</h4>
<p>
<br/>
<br/>
<a class="btn btn-highlight btn-lg" href="articles/gallery.html" role="button">Gallery</a>
<a class="btn btn-primary btn-lg" href="articles/get-started.html" role="button">Get Started</a>
</p>
</div>

</div>

<div class="panel panel-danger">
<div class="panel-body">
<strong>This package is not open-source, perhaps one day it will be.</strong>
</div>
</div>

<div style="margin-bottom:30px;">

```r
# install.packages("remotes")
remotes::install_github("devOpifex/g2r")
```

</div>

<div class="row">

<div class="col-sm-3">
<div class="panel panel-info">
<div class="panel-heading">Easy to use</div>
<div class="panel-body centerize">
An intuitive, familiar <a href="articles/aspects.html">API</a>, and easy to pick up API.
</div>
</div>
</div>

<div class="col-sm-3">
<div class="panel panel-info">
<div class="panel-heading">Crosstalk</div>
<div class="panel-body centerize">
Support for <a href="articles/crosstalk.html">crosstalk</a> selection and filtering.
</div>
</div>
</div>

<div class="col-sm-3">
<div class="panel panel-info">
<div class="panel-heading">Shiny</div>
<div class="panel-body centerize">
Integration with <a href="articles/shiny.html">shiny</a> events and proxies.
</div>
</div>
</div>

<div class="col-sm-3">
<div class="panel panel-info">
<div class="panel-heading">Rmd</div>
<div class="panel-body centerize">
Integration for dynamic charts in <a href="articles/actions.html">Rmarkdown</a>.
</div>
</div>
</div>

</div>

```r
g2(penguins, asp(flipper_length_mm, body_mass_g, color = species)) %>%  
  fig_point(
    asp(size = body_mass_g, shape = "circle"),
    fillOpacity = .5,
    stroke = 0
  ) %>%  
  fig_smooth() %>% 
  gauge_shape(c("circle", "square", "triangle")) %>% 
  axis_title_x("Flipper Length (mm)") %>% 
  axis_title_y("Body Mass (gram)") 
```

<iframe class="g2r" src="embed.html" onload="this.width='100%';this.height='505px'" style="border:none;height:505px;" />
