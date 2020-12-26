#' Gauge
#' 
#' Gauge the variables (`aspects`) used on the plot.
#' 
#' @inheritParams fig_point
#' @param ... Options to gauge variables 
#' (`aspects` defined by [asp()]).
#' @param cols Character vector of column names to gauge.
#' 
#' @examples
#' g <- g2(cars, asp(speed, dist)) %>%
#'  fig_point()
#' 
#' g %>% gauge_x(min = 0)
#' g %>% gauge_y(type = "log", title = "Log")
#' g %>% gauge_x(tickCount = 10)
#' 
#' @name gauge
#' @export 
gauge_x <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_cols(g, cols, ...)
}

#' @name gauge
#' @export 
gauge_y <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_cols(g, cols, ...)
}

#' @name gauge
#' @export 
gauge_color <- function(g, ...){
  cols <- get_aspect_names(g, "color")
  gauge_cols(g, cols, ...)
}

#' @name gauge
#' @export 
gauge_colour <- gauge_color

#' @name gauge
#' @export 
gauge_size <- function(g, ...){
  cols <- get_aspect_names(g, "size")
  gauge_cols(g, cols, ...)
}

#' @name gauge
#' @export 
gauge_cols <- function(g, cols, ...){
  args <- list(...)
  for(col in cols){
    for(i in seq_along(args)){
      g$x$scale[[col]][[names(args)[i]]] <- args[[i]]
    }
  }
  g
}

#' Get column names
#' 
#' @importFrom purrr map
#' 
#' @keywords internal
get_aspect_names <- function(g, aspect){
  g$x$views %>% 
    map(aspect) %>% 
    unlist()
}

#' Sync
#' 
#' Sets the syncs groups for x and y variables.
#' 
#' @inheritParams fig_point
#' @param position Vector of column names of length 2
#' indicating x and y columns.
#' @param sync Either a logical or the name of a group.
#' 
#' @keywords internal
sync <- function(g, position, sync = TRUE){
  
  # it's FALSE
  if(is.logical(sync) && !isTRUE(sync))
    return()

  if(isTRUE(sync)){
    syncX <- "mainGroupX"
    syncY <- "mainGroupY"
  }
  
  # sync x
  g$x$scale[[position[1]]][["sync"]] <- syncX
  g$x$scale[[position[2]]][["sync"]] <- syncY

  g
}

#' Column to type
#' 
#' Get the type from a column, used for the 
#' gauge-family of functions.
#' 
#' @param data Data.frame or tibble.
#' @param col Column name.
#' 
#' @examples
#' \dontrun{
#' col_to_type(cars, "speed")
#' col_to_type(iris, "Species")
#' }
#' 
#' @importFrom utils head
#'  
#' @keywords internal
col_to_type <- function(col, data){
  x <- head(data[[col]])

  if(inherits(x, "POSIXct") || inherits(x, "POSIXlt"))
    return("time")

  if(inherits(x, "factor") || inherits(x, "character"))
    return("cat")

  "linear"
}

#' Set Gauges Types
#' 
#' @inheritParams fig_point
#' @param cols Column names to set.
#' @param data Dataset to use to determine types.
#' 
#' @keywords internal
gauges_types <- function(g, cols, data = NULL){
  if(is.null(data))
    data <- g$x$data

  types <- sapply(cols, col_to_type, data = data)

  for(i in seq_along(types)){
    g <- gauge_cols(g, cols[i], type = types[i])
  }

  g
}
