#' Figure Tooltip
#' 
#' Control tooltip for a specific figure.
#' 
#' @param ... Aspects to use for the `callback`.
#' @param callback A JavaScript callback function as 
#' character string that accepts `asps` as input.
#' 
#' @details Define the aspects to use in the callback function.
#' This can be used in combination with [conf_tooltip()].
#' 
#' @examples 
#' # callback function
#' cb <- "(mpg, qsec) => {
#'  return {
#'    name: mpg + ' (miles per gallon)',
#'    value: qsec + ' (1/4 mile time)'
#'  }
#' }"
#' 
#' g2(mtcars, asp(mpg, qsec)) %>% 
#'   fig_point(tooltip(cb, mpg, qsec))
#' 
#' # using a template
#' template <- "<p>{mpg} (mpg): {qsec} (1/4 mile time)</p>"
#' 
#' cb <- "(mpg, qsec) => {
#'  return {
#'    mpg,
#'    qsec
#'  }
#' }"
#' 
#' g2(mtcars, asp(mpg, qsec)) %>% 
#'  fig_point(tooltip(cb, mpg, qsec)) %>% 
#'  conf_tooltip(showTitle = FALSE, itemTpl = template)
#' 
#' @importFrom rlang as_label enquos
#' 
#' @export
tooltip <- function(callback, ...) {

  if(missing(callback))
    stop("Missing `callback`", call. = FALSE)
  
  aspects <- enquos(...) %>% 
    sapply(as_label) %>% 
    unname()

  options <- list(
    paste0(aspects, collapse = "*"),
    JS(callback)
  )

  structure(options, class = c("tooltip", class(options)))
}

#' Tooltip Check
#' 
#' Checks whether the object is of class `tooltip`,
#' as returned by [tooltip()].
#' 
#' @param x Object to check.
#' 
#' @examples 
#' \dontrun{
#' is_tooltip(1)
#' is_tooltip(tooltip("callback", x, y))
#' }
#' 
#' @return A boolean.
#' 
#' @keywords internal
is_tooltip <- function(x){
  if(inherits(x, "tooltip"))
    return(TRUE)
  FALSE
}

#' Get Tooltip
#' 
#' Get any [tooltip()] from the three dot construct.
#' 
#' @param ... Any argument.
#' 
#' @examples 
#' \dontrun{
#' foo <- function(...){
#'  get_tooltip(...)
#' }
#' 
#' foo(1)
#' foo(tooltip("callback", x, y))
#' }
#' 
#' @importFrom purrr keep
#' 
#' @keywords internal
get_tooltip <- function(...){
  list(...) %>% 
    keep(is_tooltip)
}

#' @export
print.tooltip <- function(x, ...){
  cat("Tooltip: ")
  cat(paste(x$aspects, collapse = "*"), "\n")
}

#' Configure Tooltip
#' 
#' Customise the tooltip.
#' 
#' @inheritParams fig_point
#' @param ... Options to pass to the axis, pass `FALSE`
#' to hide the axis. Visit the 
#' [official documentation](https://g2.antv.vision/en/docs/api/general/tooltip)
#' for the full list of options.
#' 
#' @section Functions:
#' 
#' - `axis_x`: Customise the x axis.
#' - `axis_y`: Customise the y axis.
#' - `axis_asps`: Customise the axis by aspects (column names).
#' - `axis_hide`: Hide all axis.
#' 
#' @examples 
#' g2(mtcars, asp(drat, qsec, color = hp)) %>% 
#'  fig_point() %>% 
#'  conf_tooltip(
#'    showCrosshairs = TRUE,
#'    crosshairs = list(type = "xy")
#' )
#' 
#' @name conf_tooltip
#' @export 
conf_tooltip <- function(g, ...) UseMethod("conf_tooltip")

#' @method conf_tooltip g2r
#' @export 
conf_tooltip.g2r <- function(g, ...){
  g$x$tooltip <- list(...)
  g
}
