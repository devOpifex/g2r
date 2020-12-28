#' Interplay
#' 
#' Add interplay (interactions) to the chart.
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
#'  interplay("element", "selected")
#' 
#' @export 
interplay <- function(g, ...) UseMethod("interplay")

#' @method interplay g2r
#' @export 
interplay.g2r <- function(g, ...){
  action <- paste0(c(...), collapse = "-")
  g$x$interactions <- append(g$x$interactions, list(action))
  g
}