#' Visible
#' 
#' @export 
action_toggle_visible <- function(g, btn){
  btn <- list(btn = btn, name = "change-visibility")
  g$x$actions <- append(g$x$actions, list(btn))
  g
}

#' Button
#' 
#' Add a button input.
#' 
#' @param id Id of the button.
#' @param class Class of the button.
#' 
#' @details The `class` argument defines the style of
#' the button in Bootstrap 3, generally accepts:`for`
#' 
#' - `default`
#' - `info`
#' - `success`
#' - `warning`
#' - `danger`
#' 
#' @export 
input_button <- function(id, class = "default"){
  check_package("htmltools")

  htmltools::tags$a(
    id = id,
    class = sprintf("btn btn-", class)
  )
}

#' Slider
#' 
#' Add a slider to an R markdown document.
#' 
#' @param id Valid CSS id of the element.
#' @param label Label to display.
#' @param value Initial value of the slider.
#' @param min,max Minimum and maximum value the slider can be set.
#' @param step Interval between steps.
#' 
#' @examples
#' input_slider(
#'  "mySlider",
#'  "The label",
#'  value = 5,
#'  min = 0,
#'  max = 10
#' )
#' 
#' @export
input_slider <- function(id, label, value, min, max, step = 1){
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
      label,
      htmltools::span(
        value, 
        id = sprintf("%s-value", id)
      )
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

#' @export 
input_select <- function(id, label, choices){
  check_package("htmltools")

  opts <- lapply(choices, function(c){
    htmltools::tags$option(value = c, c)
  })

  htmltools::div(
    class = "form-group",
    htmltools::tags$label(
      class = "control-label", 
      `for` = id,
      label
    ),
    htmltools::tags$select(
      id = id,
      opts
    )
  )
}
