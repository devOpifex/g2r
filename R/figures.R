#' Bin
#' 
#' Add a bin figure to the chart.
#' 
#' @inheritParams fig_point
#' @param type The shape of bin to create.
#' @param bins The dimensions of the bins (width, height).
#' @param size_count Whether to size the binds by count.
#' 
#' @examples 
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_bin()
#' 
#' @export 
fig_bin <- function(
  g, 
  ..., 
  type = c("rectangle", "hexagon"), 
  bins = c(10, 10),
  size_count = TRUE,
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
  type = c("rectangle", "hexagon"), 
  bins = c(10, 10),
  size_count = TRUE,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){

  check_alter()

  type <- match.arg(type)
  type <- sprintf("bin.%s", type)

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")

  data <- alter::Alter$new(get_data(g, data))$
    source()$
    transform(
      sizeByCount = size_count, 
      type = type, 
      fields = position, 
      bins = bins,
      as = c(position, "count")
    )$
    getRows()

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
