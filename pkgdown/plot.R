
library(crosstalk)

data(penguins, package = "palmerpenguins")

sd <- SharedData$new(penguins)

bscols(
  g2(sd, asp(bill_length_mm, bill_depth_mm, color = species)) %>% 
    fig_point(asp(interplay = "brush", size = flipper_length_mm, shape = "circle")),
  g2(sd, asp(flipper_length_mm, body_mass_g, color = species)) %>% 
    fig_point(
      asp(
        size = bill_length_mm, 
        shape = "circle",
        interplay = "brush"
      )
    )
)
