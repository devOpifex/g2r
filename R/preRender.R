#' Pre Render Hook
#' 
#' @param g g2r obeject as returned by [g2()].
#' 
#' @return A modified version of `g`.
#' 
#' @keywords internal
pre_render <- function(g){  
  
  # filter columns
  cols <- unique(g$x$cols)
  g$x$data <- select_columns(g$x$data, cols)
  
  # remove unneeded
  g$x$cols <- NULL
  g$x$main_asp <- NULL

  g
}