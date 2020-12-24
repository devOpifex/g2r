renderG2 <- function(g){
  cols <- unique(g$x$cols)
  
  if(!is.null(g$x$data)){
    g$x$data <- select_columns(g$x$data, cols)
  }

  g
}