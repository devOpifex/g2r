#' Interplay
#' 
#' Configure global interplay (interactions) for the chart.
#' See [gauge_interplay()] to customise figure-level interplay.
#' 
#' @inheritParams fig_point
#' @param ... String(s) defining interactions.
#' 
#' @examples 
#' df <- data.frame(
#'  x = letters,
#'  y = runif(26)
#' )
#' 
#' g2(df, asp(x, y)) %>%
#'  fig_interval(
#'    selected(fill = "orange")
#'  ) %>% 
#'  conf_interplay("element", "selected")
#' 
#' # brush
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_point(asp(interplay = "brush"))
#' 
#' @name interplay
#' @export 
conf_interplay <- function(g, ...) UseMethod("conf_interplay")

#' @method conf_interplay g2r
#' @export 
conf_interplay.g2r <- function(g, ...){
  action <- paste0(c(...), collapse = "-")
  g$x$interactions <- append(g$x$interactions, list(action))
  g
}

#' @name interplay
#' @export 
remove_interplay <- function(g, ...) UseMethod("remove_interplay")

#' @method remove_interplay g2r
#' @export 
remove_interplay.g2r <- function(g, ...){
  action <- paste0(c(...), collapse = "-")
  g$x$rmInteractions <- append(g$x$rmInteractions, list(action))
  g
}
