#' Plot Action
#' 
#' Include dynamic elements in Rmarkdown.
#' 
#' @param plot_id Id of chart to interact with.
#' @param input_id Id of input that triggers the action.
#' @param ... Aspects, see [asp()].
#' @param data Data.frame containing data to plot.
#' 
#' 
#' @export
g2_action <- function(
  plot_id,
  input_id, 
  ..., 
  data = NULL
){

  if(missing(plot_id))
    stop("Missing `plot_id`", call. = FALSE)

  if(missing(input_id))
    stop("Missing `input_id`", call. = FALSE)

  action <- list(
    x = list(
      is_action = TRUE, 
      id = plot_id, # id of chart
      input_id = input_id, # id of input
      data = as_tib(data), # dataset
      main_asp = get_asp(...), # main aspects
      views = list(), # views | figures
      scale = list(), # chart.scale
      cols = c() # keep track of columns for filter
    )
  )
  
  structure(action, class = c("g2Action", "g2Proxy", class(action)))
}

#' @export 
print.g2Action <- function(x, ...){
  cat("Action for chart: ", x$id, "\n")
}

#' Button Input
#' 
#' Add a button input.
#' 
#' @param id Id of the button.
#' @param label Label to display.
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
input_button <- function(id, label, class = "default"){
  check_package("htmltools")

  if(missing(label))
    stop("Missing `label`", call. = FALSE)

  if(missing(id))
    stop("Missing `id`", call. = FALSE)

  htmltools::tags$button(
    id = id,
    class = sprintf("btn btn-", class),
    type = "button",
    label
  )
}

#' Slider Input
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

  if(missing(id))
    stop("Missing `id`", call. = FALSE)

  if(missing(label))
    stop("Missing `label`", call. = FALSE)

  if(missing(value))
    stop("Missing `value`", call. = FALSE)

  if(missing(min))
    stop("Missing `min`", call. = FALSE)

  if(missing(max))
    stop("Missing `max`", call. = FALSE)

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

#' Select Input
#' 
#' @param id Valid CSS id of the element.
#' @param label Label to display.
#' @param choices Vector of choices
#' 
#' @importFrom purrr map2
#' 
#' @export 
input_select <- function(id, label, choices){
  check_package("htmltools")

  if(missing(id))
    stop("Missing `id`", call. = FALSE)

  if(missing(label))
    stop("Missing `label`", call. = FALSE)

  if(missing(choices))
    stop("Missing `choices`", call. = FALSE)

  # allow named vector
  nms <- names(choices)
  if(is.null(nms))
    nms <- choices

  opts <- map2(choices, nms, function(c, v){
    htmltools::tags$option(value = v, c)
  })

  htmltools::div(
    class = "g2-select",
    htmltools::tags$label(
      `for` = id,
      label
    ),
    htmltools::tags$select(
      id = id,
      name = id,
      type = "select",
      opts
    )
  )
}
