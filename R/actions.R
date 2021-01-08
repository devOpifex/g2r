#' Visible
#' 
#' @export 
action_toggle_visible <- function(g, btn){
  btn <- list(
    type = "btn",
    btn = btn
  )
  g$x$actions <- append(g$x$actions, list(btn))
  g
}

#' Slider
#' 
#' @export
slider_input <- function(id, label, value, min, max, step = 1){
  check_package("htmltools")

  dep <- htmltools::htmlDependency(
    "slider",
    version = "1.0.0",
    src = "actions",
    script = c(file = "slider.js"), 
    package = "g2r"
  )  

  htmltools::div(
    dep,
    class = "g2-slider",
    htmltools::tags$label(
      `for` = id,
      label
    ),
    htmltools::tags$input(
      type = "range", 
      id = id,
      name = id,
      min = min,
      max = max,
      value = value,
      step = step
    )
  )
}