library(g2r)

setwd("./pkgdown")

data(penguins, package = "palmerpenguins")

g <-g2(penguins, asp(flipper_length_mm, body_mass_g, color = species)) %>%  
  fig_point(
    asp(size = body_mass_g, shape = "circle"),
    fillOpacity = .5,
    stroke = 0
  ) %>%  
  fig_smooth() %>% 
  axis_title_x("Flipper Length (mm)") %>% 
  axis_title_y("Body Mass (gram)") %>% 
  axis_y(title = list(offset = 50)) %>% 
  gauge_color_g2rq()
  # htmlwidgets::prependContent(
  #   htmltools::tags$style(
  #     "body{
  #       background-color: white;
  #     }
  #     @media (prefers-color-scheme: dark) {
  #       body{
  #         background-color: #1e024a;
  #       }
  #     }"
  #   )
  # )

htmlwidgets::saveWidget(g, "embed.html")

widget <- readLines("embed.html")

body_index <- purrr::map(widget, function(x){
  grepl("<body", x)
}) %>% 
  unlist()

widget[body_index] <- "<body>"

writeLines(widget, "embed.html")

fs::file_copy("embed.html", "../docs", overwrite = TRUE)

setwd("../")
