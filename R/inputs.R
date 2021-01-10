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
    class = sprintf("btn btn-%s", class),
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
    script = c("slider.js", "inputs.js"), 
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

insert_empty <- function(){
  return("")
}

#' Checkbox and Radio
#' 
#' Add a checkbox or radio input.
#' 
#' @param id Id of input.
#' @param label Label of the input.
#' @param choices Vector of choices to define
#' either the checboxes or radio inputs.
#' @param selected Vector of `choices` that are
#' selected by default.
#' @param inline Whether the input is inline.
#' 
#' @export 
input_checkbox <- function(
  id, 
  label, 
  choices, 
  selected = NULL,
  inline = TRUE
){
  input_tick(
    id, 
    label, 
    choices, 
    inline, 
    selected, 
    type = "checkbox"
  )
}

#' @export 
input_radio <- function(
  id, 
  label, 
  choices, 
  selected = NULL,
  inline = TRUE
){
  input_tick(
    id, 
    label, 
    choices, 
    inline, 
    selected, 
    type = "radio"
  )
}

#' @keywords internal
input_tick <- function(
  id, 
  label, 
  choices, 
  inline = TRUE,
  selected = NULL,  
  type = c("checkbox", "radio")
){

  type <- match.arg(type)

  linebreak <- insert_empty
  wrapper <- htmltools::span
  if(!inline){
    linebreak <- htmltools::br
    wrapper <- htmltools::p
  }

  # allow named vector
  nms <- names(choices)
  if(is.null(nms))
    nms <- choices

  opts <- map2(choices, nms, function(c, v, lab, br, type){
    htmltools::tags$label(
      htmltools::tags$input(
        type = type, 
        value = v,
        name = lab, 
        c,
        br()
      )
    )
  }, lab = id, br = linebreak, type = type)

  opts <- map(opts, function(opt, sel){
    value <- htmltools::tagGetAttribute(
      opt$children[[1]], 
      "value"
    )

    if(value %in% sel)
      opt$children[[1]] <- htmltools::tagAppendAttributes(
        opt$children[[1]],
        checked = NA
      )

    return(opt) 
  }, sel = selected)

  htmltools::div(
    class = sprintf("g2-%s", type),
    id = id,
    wrapper(label),
    opts
  )
}
