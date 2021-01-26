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
#' @importFrom htmltools tags
#' 
#' @export
input_button <- function(id, label, class = "default") {

  if (missing(label)) {
    stop("Missing `label`", call. = FALSE)
  }

  if (missing(id)) {
    stop("Missing `id`", call. = FALSE)
  }

  tags$button(
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
#'   "mySlider",
#'   "The label",
#'   value = 5,
#'   min = 0,
#'   max = 10
#' )
#' 
#' @importFrom htmltools tags span div htmlDependency
#' 
#' @export
input_slider <- function(id, label, value, min, max, step = 1) {

  if (missing(id)) {
    stop("Missing `id`", call. = FALSE)
  }

  if (missing(label)) {
    stop("Missing `label`", call. = FALSE)
  }

  if (missing(value)) {
    stop("Missing `value`", call. = FALSE)
  }

  if (missing(min)) {
    stop("Missing `min`", call. = FALSE)
  }

  if (missing(max)) {
    stop("Missing `max`", call. = FALSE)
  }

  dep <- htmlDependency(
    "slider",
    version = "1.0.0",
    src = "actions",
    script = c("slider.js", "inputs.js"),
    package = "g2r"
  )

  div(
    dep,
    class = "g2-slider",
    tags$label(
      `for` = id,
      label,
      span(
        value,
        id = sprintf("%s-value", id)
      )
    ),
    tags$input(
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
#' @importFrom htmltools tags div
#'
#' @export
input_select <- function(id, label, choices) {

  if (missing(id)) {
    stop("Missing `id`", call. = FALSE)
  }

  if (missing(label)) {
    stop("Missing `label`", call. = FALSE)
  }

  if (missing(choices)) {
    stop("Missing `choices`", call. = FALSE)
  }

  # allow named vector
  nms <- names(choices)
  if (is.null(nms)) {
    nms <- choices
  }

  opts <- map2(choices, nms, function(c, v) {
    tags$option(value = v, c)
  })

  div(
    class = "g2-select",
    tags$label(
      `for` = id,
      label
    ),
    tags$select(
      id = id,
      name = id,
      type = "select",
      opts
    )
  )
}

insert_empty <- function() {
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
#' @name radio
#' @export
input_checkbox <- function(
  id,
  label,
  choices,
  selected = NULL,
  inline = TRUE
) {
  input_tick(
    id,
    label,
    choices,
    inline,
    selected,
    type = "checkbox"
  )
}

#' @rdname radio
#' @export
input_radio <- function(
  id,
  label,
  choices,
  selected = NULL,
  inline = TRUE
) {
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
) {
  type <- match.arg(type)

  linebreak <- insert_empty
  wrapper <- htmltools::span
  if (!inline) {
    linebreak <- htmltools::br
    wrapper <- htmltools::p
  }

  # allow named vector
  nms <- names(choices)
  if (is.null(nms)) {
    nms <- choices
  }

  opts <- map2(choices, nms, function(c, v, lab, br, type) {
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

  opts <- map(opts, function(opt, sel) {
    value <- htmltools::tagGetAttribute(
      opt$children[[1]],
      "value"
    )

    if (value %in% sel) {
      opt$children[[1]] <- htmltools::tagAppendAttributes(
        opt$children[[1]],
        checked = NA
      )
    }

    return(opt)
  }, sel = selected)

  htmltools::div(
    class = sprintf("g2-%s", type),
    id = id,
    wrapper(label),
    opts
  )
}
