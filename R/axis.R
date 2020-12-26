#' Axes
#' 
#' Customise the axes.
#' 
#' @inheritParams fig_point
#' @param ... Options to pass to the axis, passe `FALSE`
#' to hide the axis.
#' @param cols Column names to change the axis of.
#' 
#' @section Functions:
#' 
#' - `axis_x`: Customise the x axis.
#' - `axis_y`: Customise the y axis.
#' - `axis_cols`: Customise the axis by column names.
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
#' g %>% axis_cols("speed", FALSE)
#' 
#' # change position
#' g %>% axis_x(position = "top")
#' 
#' @name axis
#' @export 
axis_x <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  axis_cols(g, cols, ...)
}

#' @name axis
#' @export 
axis_y <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  axis_cols(g, cols, ...)
}

#' @name axis
#' @export 
axis_cols <- function(g, cols, ...){
  if(missing(cols))
    stop("Missing `cols`", call. = FALSE)

  axis <- lapply(cols, function(c, opts){

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

#' @name axis
#' @export 
axis_hide <- function(g){
  g %>% 
    axis_x(FALSE) %>% 
    axis_y(FALSE)
}
