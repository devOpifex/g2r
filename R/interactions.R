#' Interactions
#' 
#' Add interactions to the chart.
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
#'  interaction("element", "selected")
#' 
#' @export 
interaction <- function(g, ...) UseMethod("interaction")

#' @method interaction g2r
#' @export 
interaction.g2r <- function(g, ...){
  action <- paste0(c(...), collapse = "-")
  g$x$interactions <- append(g$x$interactions, list(action))
  g
}