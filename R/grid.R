#' Axes
#' 
#' Customise the axes.
#' 
#' @inheritParams fig_point
#' @param ... Options to pass to the axis.
#' @param cols Column names to change the axis of.
#' 
#' @section Functions:
#' 
#' - `axis_x`: Customise the x axis.
#' - `axis_y`: Customise the y axis.
#' - `axis_cols`: Customise the axis by column names.
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
#' @importFrom purrr map
#' 
#' @name axis
#' @export 
axis_x <- function(g, ...){
  cols <- get_axis_column_names(g, "x")
  axis_cols(g, cols, ...)
}

#' @name axis
#' @export 
axis_y <- function(g, ...){
  cols <- get_axis_column_names(g, "y")
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

#' Retrieve Column Names
#' 
#' Retrieve column names used for position (x, and y).
#' 
#' @inheritParams fig_point
#' @param axis Axis to retrieve the used column names.
#' 
#' @keywords internal
get_axis_column_names <- function(g, axis = c("x", "y")){
  axis <- match.arg(axis)

  index <- 1
  if(axis == "y")
    index <- 2

  map(g$x$views, "position") %>% 
    map(function(x, i){
      x[i]
    }, i = index) %>% 
    unlist()
}
