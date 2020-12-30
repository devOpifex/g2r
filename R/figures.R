#' Bin
#' 
#' Add a bin figure to the chart.
#' 
#' @inheritParams fig_point
#' @param type The shape of bin to create.
#' @param bins Number of bins by dimension (width, height).
#' @param size_count Whether to size the binds by count.
#' 
#' @examples 
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_bin(size_count = FALSE)
#' 
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_bin(type = "hexagon")
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

#' Ribbon
#' 
#' Add a ribbon figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @examples 
#' df <- data.frame(
#'  x = 1:100,
#'  ymin = runif(100, 1, 5),
#'  ymax = runif(100, 6, 13)
#' )
#' 
#' g2(df, asp(x, ymin = ymin, ymax = ymax)) %>% 
#'  fig_ribbon()
#' 
#' @export 
fig_ribbon <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  UseMethod("fig_ribbon")
}

#' @method fig_ribbon g2r
#' @export 
fig_ribbon.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  cols <- select_asp_labels(asp, "ymin", "ymax")

  data <- get_data(g, data)

  range <- purrr::pmap(data, list) %>% 
    purrr::map(function(row, cols){
      list(row[[cols[1]]], row[[cols[2]]])
    }, cols = cols)

  data$range <- range
  asp$y <- "range"

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "area",
    style = style,
    asp = asp
  )
}

#' Histogram
#' 
#' Add a histogram figure to the chart.
#' 
#' @inheritParams fig_point
#' @param bin_width Width of bin.
#' 
#' @examples 
#' df <- data.frame(
#'  grp = rep(c("A", "B"), each = 200),
#'    val = c(
#'      rnorm(200, mean = 57, sd = 5), 
#'      rnorm(200, mean = 53, sd = 5)
#'    )
#' )
#' 
#' g2(df, asp(val, color = grp)) %>% 
#'  fig_histogram(adjust("stack"), bin_width = 1)
#' 
#' @export 
fig_histogram <- function(
  g, 
  ..., 
  bin_width = 5,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  UseMethod("fig_histogram")
}

#' @method fig_histogram g2r
#' @export 
fig_histogram.g2r <- function(
  g, 
  ..., 
  bin_width = 5,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){

  check_alter()

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  x <- select_asp_labels(asp, "x")
  color <- select_asp_labels(asp, "color")

  if(!length(color))
    data <- alter::Alter$new(get_data(g, data))$
      source()$
      transform(
        type = "bin.histogram",
        field = x,
        binWidth = bin_width,
        as = c(x, "count")
      )
  else
    data <- alter::Alter$new(get_data(g, data))$
      source()$
      transform(
        type = "bin.histogram",
        field = x,
        binWidth = bin_width,
        groupBy = as.list(color),
        as = c(x, "count")
      )
    
  data <- data$getRows()

  asp$y <- "count"

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "interval",
    style = style,
    asp = asp
  )
}