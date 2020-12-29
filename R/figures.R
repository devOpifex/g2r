#' Bin
#' 
#' @export 
fig_bin <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  UseMethod("fig_bin")
}

#' @method fig_bin g2r
#' @export 
fig_bin.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){

  check_alter()

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "polygon",
    style = style
  )
}
