#' Series
#' 
#' @export 
g_bar <- function(g, ..., inherit_asp = TRUE){
  asp <- get_asp(...)
  asp <- combine_asp(g$x$main_asp, asp, inherit_asp = inherit_asp)

  position <- select_asp_labels(asp, "x", "y")
  
  opts <- list(
    type = "bar",
    position = position
  )

  g$x$views <- append(g$x$views, list(opts))

  g
}