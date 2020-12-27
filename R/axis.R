#' Axes
#' 
#' Customise the axes.
#' 
#' @inheritParams fig_point
#' @param ... Options to pass to the axis, passe `FALSE`
#' to hide the axis.
#' @param asps Column names to change the axis of.
#' 
#' @section Functions:
#' 
#' - `axis_x`: Customise the x axis.
#' - `axis_y`: Customise the y axis.
#' - `axis_asp`: Customise the axis by aspects (column names).
#' - `axis_hide`: Hide all axis.
#' 
#' @examples 
#' g <- g2(cars, asp(speed, dist)) %>% 
#'  fig_point()
#' 
#' # hide axis
#' g %>% axis_x(FALSE)
#' 
#' # same as above
#' g %>% axis_asps("speed", FALSE)
#' 
#' # change position
#' g %>% axis_x(position = "top")
#' 
#' @name axis
#' @export 
axis_x <- function(g, ...) UseMethod("axis_x")

#' @method axis_x g2r
#' @export 
axis_x.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  axis_asps(g, cols, ...)
}

#' @rdname axis
#' @export 
axis_y <- function(g, ...) UseMethod("axis_y")

#' @method axis_y g2r
#' @export 
axis_y.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  axis_asps(g, cols, ...)
}

#' @rdname axis
#' @export 
axis_asps <- function(g, asps, ...) UseMethod("axis_asps")

#' @method axis_asps g2r
#' @export 
axis_asps.g2r <- function(g, asps, ...){
  if(missing(asps))
    stop("Missing `asps`", call. = FALSE)

  axis <- lapply(asps, function(c, opts){

    if(is.logical(opts[[1]]))
      opts <- opts[[1]]

    list(
      column = c,
      opts = opts
    )
  }, opts = list(...))

  g$x$axis <- append(g$x$axis, axis)

  g
}

#' @rdname axis
#' @export 
axis_hide <- function(g) UseMethod("axis_hide")

#' @method axis_hide g2r
#' @export
axis_hide.g2r <- function(g){
  g %>% 
    axis_x(FALSE) %>% 
    axis_y(FALSE)
}
