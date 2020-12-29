#' Gauge Aspects
#'
#' Customise aspects of the chart.
#'  
#' @seealso [gauge] to gauge aspects of the grid and axis.
#' 
#' @inheritParams fig_point
#' @param handler Handler to customise the aspect.
#' Generally a vector or a function (wrapped in `htmlwidgets::JS`).
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
#' cb <- "function(mpg){
#'  if(mpg > 25){
#'    return 'blue';
#'  }
#' 
#'  return 'red';
#' }"
#' 
#' g %>% gauge_color(htmlwidgets::JS(cb))
#' 
#' @name gaugeViews
#' @export 
gauge_color <- function(g, handler) UseMethod("gauge_color")

#' @method gauge_color g2r
#' @export 
gauge_color.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "color")
}

#' @rdname gaugeViews
#' @export 
gauge_size <- function(g, handler) UseMethod("gauge_size")

#' @method gauge_size g2r
#' @export 
gauge_size.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "size")
}

#' @rdname gaugeViews
#' @export 
gauge_shape <- function(g, handler) UseMethod("gauge_shape")

#' @method gauge_shape g2r
#' @export 
gauge_shape.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "shape")
}

#' @rdname gaugeViews
#' @export 
gauge_label <- function(g, handler) UseMethod("gauge_label")

#' @method gauge_label g2r
#' @export 
gauge_label.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "shape")
}

#' @rdname gaugeViews
#' @export 
gauge_tooltip <- function(g, handler) UseMethod("gauge_tooltip")

#' @method gauge_tooltip g2r
#' @export 
gauge_tooltip.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "tooltip")
}

#' @rdname gaugeViews
#' @export 
gauge_label <- function(g, handler) UseMethod("gauge_label")

#' @method gauge_label g2r
#' @export 
gauge_label.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "label")
}

#' @rdname gaugeViews
#' @export 
gauge_style <- function(g, handler) UseMethod("gauge_style")

#' @method gauge_style g2r
#' @export 
gauge_style.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "style")
}

#' @rdname gaugeViews
#' @export 
gauge_interplay <- function(g, handler) UseMethod("gauge_interplay")

#' @method gauge_interplay g2r
#' @export 
gauge_interplay.g2r <- function(g, handler){
  gauge2_(g, handler, fn = "interaction")
}

#' Gauge2
#' 
#' While [gauge_()] applies the `scale` to the
#' global `chart` object, [gauge2_()] applies it to the 
#' `view`.
#' 
#' @inheritParams fig_point
#' @param handler Handler to pass as second argument to
#' the JavaScript function.
#' @param fn Name of the function.
#' 
#' @keywords internal
gauge2_ <- function(g, handler, fn){
  if(missing(handler))
    stop("Missing `handler`", call. = FALSE)

  if(missing(fn))
    stop("Missing `fn`", call. = FALSE)

  for(i in 1:length(g$x$views)){

    if(is.null(g$x$views[[i]][[fn]]))
      next

    if(is.logical(g$x$views[[i]][[fn]]))
      next

    if(is.logical(handler))
      g$x$views[[i]][[fn]] <- handler
    else 
      g$x$views[[i]][[fn]] <- list(
        g$x$views[[i]][[fn]], handler
      )
  }
  g
}