#' Gauge Aspects
#'
#' Customise aspects of the chart.
#'  
#' @seealso [gauge] to gauge aspects of the grid and axis.
#' 
#' @inheritParams fig_point
#' @param ... Arguments to customise the gauge.
#' Generally, key value pairs of options, a vector of hex colors,
#' or a JavaScript function (wrapped in [htmlwidgets::JS()]).
#' 
#' @examples 
#' # base plot
#' g <- g2(cars, asp(speed, dist)) %>% 
#'  fig_point(asp(color = speed)) 
#' 
#' # color with vector
#' g %>% gauge_color(c("red", "white", "blue"))
#' 
#' # color with callback
#' cb <- "function(speed){
#'  if(speed > 10){
#'    return 'blue';
#'  }
#'  return 'red';
#' }"
#' 
#' g %>% gauge_color(htmlwidgets::JS(cb))
#' 
#' @name gaugeViews
#' @export 
gauge_color <- function(g, ...) UseMethod("gauge_color")

#' @method gauge_color g2r
#' @export 
gauge_color.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "color")
}

#' @rdname gaugeViews
#' @export 
gauge_size <- function(g, ...) UseMethod("gauge_size")

#' @method gauge_size g2r
#' @export 
gauge_size.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "size")
}

#' @rdname gaugeViews
#' @export 
gauge_shape <- function(g, ...) UseMethod("gauge_shape")

#' @method gauge_shape g2r
#' @export 
gauge_shape.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "shape")
}

#' @rdname gaugeViews
#' @export 
gauge_label <- function(g, ...) UseMethod("gauge_label")

#' @method gauge_label g2r
#' @export 
gauge_label.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "shape")
}

#' @rdname gaugeViews
#' @export 
gauge_tooltip <- function(g, ...) UseMethod("gauge_tooltip")

#' @method gauge_tooltip g2r
#' @export 
gauge_tooltip.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "tooltip")
}

#' @rdname gaugeViews
#' @export 
gauge_label <- function(g, ...) UseMethod("gauge_label")

#' @method gauge_label g2r
#' @export 
gauge_label.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "label")
}

#' @rdname gaugeViews
#' @export 
gauge_style <- function(g, ...) UseMethod("gauge_style")

#' @method gauge_style g2r
#' @export 
gauge_style.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "style")
}

#' @rdname gaugeViews
#' @export 
gauge_interplay <- function(g, ...) UseMethod("gauge_interplay")

#' @method gauge_interplay g2r
#' @export 
gauge_interplay.g2r <- function(g, ...){
  gauge2_(g, ..., fn = "interaction")
}

#' Gauge2
#' 
#' While [gauge_()] applies the `scale` to the
#' global `chart` object, [gauge2_()] applies it to the 
#' `view`.
#' 
#' @inheritParams fig_point
#' @param ... Arguments to customise the gauge.
#' Generally, key value pairs of options, a vector of hex colors,
#' or a JavaScript function (wrapped in [htmlwidgets::JS()]).
#' @param fn Name of the function.
#' 
#' @keywords internal
gauge2_ <- function(g, ..., fn){
  handler <- list(...)

  if(!length(handler))
    stop("Must pass args to `...`", call. = FALSE)

  if(missing(fn))
    stop("Missing `fn`", call. = FALSE)

  for(i in 1:length(g$x$views)){

    if(is.null(g$x$views[[i]][[fn]]))
      next

    if(is.logical(g$x$views[[i]][[fn]]))
      next

    if(length(handler) == 1 && is.null(names(handler)))
      handler <- handler[[1]]

    if(is.logical(handler[[1]]) && is.null(names(handler)))
      g$x$views[[i]][[fn]] <- handler
    else 
      g$x$views[[i]][[fn]] <- list(
        g$x$views[[i]][[fn]], handler
      )
  }
  g
}