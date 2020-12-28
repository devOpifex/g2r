#' Slider
#' 
#' Add a slider to the chart.
#' 
#' @inheritParams fig_point
#' @param ... Options to pass to the slider.
#' 
#' @seealso The [official documentation](https://g2.antv.vision/en/docs/api/general/slider)
#' for the list options to pass to `...`.
#' 
#' @export 
slider <- function(g, ...) UseMethod("slider")

#' @method slider g2r
#' @export 
slider.g2r <- function(g, ...){
  opts <- list(...)

  if(is.logical(opts[[1]]))
    opts <- FALSE

  g$x$slider <- opts
  g
}