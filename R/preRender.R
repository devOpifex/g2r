#' Pre Render Hook
#' 
#' @keywords internal
renderG2 <- function(g){
  cols <- unique(g$x$cols)
  
  g$x$data <- select_columns(g$x$data, cols)
  
  g$x$cols <- NULL

  g
}