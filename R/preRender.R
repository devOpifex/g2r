#' Pre Render Hook
#' 
#' @param g g2r obeject as returned by [g2()].
#' 
#' @return A modified version of `g`.
#' 
#' @keywords internal
pre_render <- function(g){  
  
  # filter columns
  cols <- unique(g$x$cols) %>% unlist()
  g$x$data <- select_columns(g$x$data, cols)
  
  # remove unneeded
  g$x$cols <- NULL
  g$x$main_asp <- NULL
  g$x$graph <- NULL

  # globals
  if(is.null(g$x$theme))
    g$x$theme <- get_global_theme()
  
  if(is.null(g$x$chartOpts))
    g$x$chartOpts <- get_global_chart_opts()

  # add default select if none specified
  if(is.null(g$x$crosstalk_select))
    g <- crosstalk_select(g, "stroke", "black")

  g
}