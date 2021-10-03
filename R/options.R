#' Slider
#'
#' Add a slider to the chart.
#'
#' @inheritParams fig_point
#' @param ... Options to pass to the slider.
#'
#' @seealso The [official documentation](https://g2.antv.vision/en/docs/api/general/slider/)
#' for the list options to pass to `...`.
#'
#' @export
slider <- function(g, ...) UseMethod("slider")

#' @method slider g2r
#' @export
slider.g2r <- function(g, ...) {
  opts <- list(...)

  if (!length(opts)) {
    stop("Must pass options to `...`", call. = FALSE)
  }

  if (is.logical(opts[[1]])) {
    opts <- FALSE
  }

  g$x$slider <- opts
  g
}

#' Scrollbar
#'
#' Add a scrollbar to the chart.
#'
#' @inheritParams fig_point
#' @param ... Options to pass to the slider.
#'
#' @seealso The [official documentation](https://g2.antv.vision/en/docs/api/general/scrollbar/)
#' for the list options to pass to `...`.
#'
#' @export
scrollbar <- function(g, ...) UseMethod("scrollbar")

#' @method scrollbar g2r
#' @export
scrollbar.g2r <- function(g, ...) {
  opts <- list(...)

  if (!length(opts)) {
    stop("Must pass options to `...`", call. = FALSE)
  }

  if (is.logical(opts[[1]])) {
    opts <- FALSE
  }

  g$x$scrollbar <- opts
  g
}
