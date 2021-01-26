#' Tooltip
#'
#' Configure the tooltip applied to the entire chart.
#' See [gauge_tooltip()] to customise a specific tooltip
#' (the tooltip of a specific figure).
#'
#' @inheritParams fig_point
#' @param ... Options to pass to the axis, pass `FALSE`
#' to hide the axis. Visit the
#' [official documentation](https://g2.antv.vision/en/docs/api/general/tooltip)
#' for the full list of options.
#'
#' @examples
#' g2(mtcars, asp(drat, qsec, color = hp)) %>%
#'   fig_point() %>%
#'   tooltip(
#'     showCrosshairs = TRUE,
#'     crosshairs = list(type = "xy")
#'   )
#' @name tooltip
#' @export
tooltip <- function(g, ...) UseMethod("tooltip")

#' @method tooltip g2r
#' @export
tooltip.g2r <- function(g, ...) {
  g$x$tooltip <- list(...)
  g
}

#' Tooltip Template
#'
#' Convenience function to create tooltip templates
#' (`itemTp` argument in [tooltip()] function).
#'
#' @param ... One or more [tpl_item()].
#' @param name,value Name and value of the tooltip item.
#' @param marker Whether to include the color marker (dot)
#' in the tooltip.
#'
#' @details The arguments `title`, `name`, and `value` accept
#' either a bare column name from the data to use as
#' `{mustache}`/`{handlebar}` in the template. If a string is
#' passed then it is treated as constant.
#'
#' @examples
#' template <- tpl(
#'   tpl_item(
#'     island,
#'     bill_depth_mm
#'   )
#' )
#' @importFrom rlang enquo quo_is_symbolic as_label
#' @importFrom htmltools tags
#'
#' @name template
#' @export
tpl <- function(...) {

  ul <- tags$ul(
    class = "g2-tooltip-list",
    ...
  )

  as.character(ul)
}

#' @rdname template
#' @export
#' @importFrom htmltools tags span tagAppendChild
tpl_item <- function(name, value, marker = TRUE) {

  if (missing(name)) {
    stop("Missing `name`", call. = FALSE)
  }

  if (missing(value)) {
    stop("Missing `value`", call. = FALSE)
  }

  name_enquo <- enquo(name)
  if (quo_is_symbolic(name_enquo)) {
    name <- as_label(name_enquo)
    name <- sprintf("{%s}", name)
  }

  value_enquo <- enquo(value)
  if (quo_is_symbolic(value_enquo)) {
    value <- as_label(value_enquo)
    value <- sprintf("{%s}", value)
  }

  li <- tags$li(class = "g2-tooltip-list-item")

  if (marker) {
    marker <- span(
      style = "background-color:{color};",
      class = "g2-tooltip-marker"
    )
    li <- tagAppendChild(li, marker)
  }

  # add name
  li <- tagAppendChild(
    li,
    span(
      class = "g2-tooltip-name",
      name
    )
  )

  # add value
  li <- tagAppendChild(
    li,
    span(
      class = "g2-tooltip-value",
      value
    )
  )

  li
}
