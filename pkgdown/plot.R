library(g2r)

setwd("./pkgdown")

data(penguins, package = "palmerpenguins")

g <- g2(penguins, asp(flipper_length_mm, body_mass_g, color = species)) %>% 
  fig_point(
    asp(
      size = body_mass_g, 
      shape = species
    )
  ) %>% 
  fig_smooth() %>% 
  gauge_shape(c("circle", "square", "triangle")) %>% 
  axis_title_x("Flipper Length (mm)") %>% 
  axis_title_y("Body Maxx (gram)") %>% 
  axis_y(title = list(offset = 50))

htmlwidgets::saveWidget(g, "embed.html")

fs::file_copy("embed.html", "../docs/dev", overwrite = TRUE)
fs::file_copy("embed.html", "../docs", overwrite = TRUE)

setwd("../")
