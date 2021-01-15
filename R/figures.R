#' Bin
#' 
#' Add a bin figure to the chart.
#' 
#' @inheritParams fig_point
#' @param type The shape of bin to create.
#' @param bins Number of bins by dimension (width, height).
#' @param size_count Whether to size the binds by count.
#' 
#' @details Requires the `x` and `y` aspects.
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
  inherit_asp = TRUE
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
  inherit_asp = TRUE
){
  fig_bin_(
    g, 
    ..., 
    type = type, 
    bins = bins,
    size_count = size_count,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_bin g2Proxy
#' @export 
fig_bin.g2Proxy <- function(
  g, 
  ..., 
  type = c("rectangle", "hexagon"), 
  bins = c(10, 10),
  size_count = TRUE,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_bin_(
    g, 
    ..., 
    type = type, 
    bins = bins,
    size_count = size_count,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Bin figure
#' 
#' @inheritParams fig_bin
#' 
#' @keywords internal
fig_bin_ <- function(
  g, 
  ..., 
  type = c("rectangle", "hexagon"), 
  bins = c(10, 10),
  size_count = TRUE,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
) {

  check_alter()

  type <- match.arg(type)
  type <- sprintf("bin.%s", type)

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")

  if(length(position) < 2)
    stop("Must pass `x` and `y` aspects", call. = FALSE)

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
    type = "polygon"
  )
}

#' Ribbon
#' 
#' Add a ribbon figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @details Requires the `ymin` and `ymax` aspects.
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
  inherit_asp = TRUE
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
  inherit_asp = TRUE
){
  fig_ribbon_(
    g, 
    ..., 
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_ribbon g2Proxy
#' @export 
fig_ribbon.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_ribbon_(
    g, 
    ..., 
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Fig ribbon
#' 
#' @inheritParams fig_ribbon
#' 
#' @importFrom purrr pmap map
#' 
#' @keywords internal
fig_ribbon_ <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  cols <- select_asp_labels(asp, "ymin", "ymax")

  if(length(cols) < 2)
    stop("Must pass `ymin` and `ymax` aspects", call. = FALSE)

  data <- get_data(g, data)

  range <- pmap(data, list) %>% 
    map(function(row, col){
      list(row[[col[2]]], row[[col[1]]])
    }, col = cols)

  data$range <- range
  asp$y <- "range"

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "area",
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
  inherit_asp = TRUE
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
  inherit_asp = TRUE
){
  fig_histogram_(
    g, 
    ..., 
    bin_width = bin_width,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_histogram g2Proxy
#' @export 
fig_histogram.g2Proxy <- function(
  g, 
  ..., 
  bin_width = 5,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_histogram_(
    g, 
    ..., 
    bin_width = bin_width,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Histogram
#' 
#' @inheritParams fig_histogram
#' 
#' @keywords internal
fig_histogram_ <- function(
  g, 
  ..., 
  bin_width = 5,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
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
    asp = asp
  )
}

#' Boxplot
#' 
#' Add a boxplot figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @examples 
#' # wide to long
#' # tidyr::pivot_longer(iris, -Species)
#' df <- reshape(
#'  iris, 
#'  varying = names(iris)[1:4], 
#'  direction = "long", 
#'  v.names = "value", 
#'  idvar = "Species", 
#'  new.row.names = 1:600,
#'  timevar = "var", 
#'  times = names(iris)[1:4]
#' )
#' 
#' g2(df, asp(var, value, color = Species)) %>% 
#'  fig_boxplot(adjust("dodge"))
#' 
#' g2(iris, asp(y = Sepal.Length, color = Species)) %>% 
#'  fig_boxplot(adjust("dodge"))
#' 
#' g2(iris, asp(x = Species, y = Sepal.Length, color = Species)) %>% 
#'  fig_boxplot(adjust("dodge"))
#' 
#' @export 
fig_boxplot <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_boxplot")
}

#' @method fig_boxplot g2r
#' @export 
fig_boxplot.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_boxplot_(
    g, 
    ..., 
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_boxplot g2Proxy
#' @export 
fig_boxplot.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_boxplot_(
    g, 
    ..., 
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Boxplot
#' 
#' @inheritParams fig_boxplot
#' 
#' @keywords internal
fig_boxplot_ <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  check_alter()

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  x <- select_asp_labels(asp, "x")
  y <- select_asp_labels(asp, "y")
  color <- select_asp_labels(asp, "color")

  data <- alter::Alter$new(get_data(g, data))$
    source()$
    transform(
      type = 'bin.quantile',
      field = y,
      as = 'bin',
      groupBy = c(x, color)
    )$
    getRows()

  asp$y <- "bin"
  asp$shape <- "box"

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "schema",
    asp = asp
  )
}

#' Smooth
#' 
#' Add a smooth(ing) figure to the chart.
#' 
#' @inheritParams fig_point
#' @param method Smoothing method to use.
#' @param band_width Step size for Silverman's algorithm.
#' 
#' @details Requires the `x` and `y` aspects. 
#' 
#' This is a convenience function for a quick smoothing, see
#' the online documentation to see how to use your own model
#' for more control.
#' 
#' @examples 
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_point() %>% 
#'  fig_smooth(method = "gaussian")
#' 
#' g2(iris, asp(Sepal.Width, Sepal.Length, color = Species)) %>% 
#'  fig_point() %>% 
#'  fig_smooth()
#' 
#' @importFrom purrr map
#' 
#' @export 
fig_smooth <- function(
  g, 
  ..., 
  method = c(
    "linear",
    "gaussian",
    "cosine",
    "epanechnikov",
    "quartic",
    "triangular",
    "tricube",
    "triweight",
    "uniform",
    "polynomial",
    "logarithmic",
    "power",
    "polynomial",
    "exponential"
  ),
  band_width = 1,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_smooth")
}

#' @method fig_smooth g2r
#' @export 
fig_smooth.g2r <- function(
  g, 
  ..., 
  method = c(
    "linear",
    "gaussian",
    "cosine",
    "epanechnikov",
    "quartic",
    "triangular",
    "tricube",
    "triweight",
    "uniform",
    "polynomial",
    "logarithmic",
    "power",
    "polynomial",
    "exponential"
  ),
  band_width = 1,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_smooth_(
    g, 
    ..., 
    method = method,
    band_width = band_width,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_smooth g2Proxy
#' @export 
fig_smooth.g2Proxy <- function(
  g, 
  ..., 
  method = c(
    "linear",
    "gaussian",
    "cosine",
    "epanechnikov",
    "quartic",
    "triangular",
    "tricube",
    "triweight",
    "uniform",
    "polynomial",
    "logarithmic",
    "power",
    "polynomial",
    "exponential"
  ),
  band_width = 1,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_smooth_(
    g, 
    ..., 
    method = method,
    band_width = band_width,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Smooth
#' 
#' @inheritParams fig_smooth
#' 
#' @keywords internal
fig_smooth_ <- function(
  g, 
  ..., 
  method = c(
    "linear",
    "gaussian",
    "cosine",
    "epanechnikov",
    "quartic",
    "triangular",
    "tricube",
    "triweight",
    "uniform",
    "polynomial",
    "logarithmic",
    "power",
    "polynomial",
    "exponential"
  ),
  band_width = 1,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
) {

  check_alter()

  methods <- c(
    "linear",
    "polynomial",
    "logarithmic",
    "power",
    "polynomial",
    "exponential"
  )

  # method and type for alter
  method <- match.arg(method)
  type <- "kernel-smooth.regression"
  if(method %in% methods)
    type <- "regression"

  # aspects
  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")
  color <- select_asp_labels(asp, "color")

  if(length(position) < 2)
    stop("Must pass `x` and `y` aspects", call. = FALSE)

  # get data for split
  data <- get_data(g, data)

  # remove NAs to not break transform
  data <- data[stats::complete.cases(data[, unlist(c(position, color))]), ]

  lvls <- NULL
  if(length(color)){
    lvls <- unique(data[[color]])
    data <- split(data, data[[color]])
  } else {
    data <- list(data)
  }

  df <- map(
    data, 
    function(
      df, 
      color,
      type,
      method,
      bandwidth,
      position
    ){

      dat <- alter::Alter$new(df)$
        source()$
        transform(
          type = type,
          method = method,
          bandwidth = band_width,
          fields = position,
          as = position
        )$
        getRows()

      if(length(color) && !color %in% position)
        dat[[color]] <- unique(df[[color]])

      return(dat)
  }, 
    color = color,
    type = type,
    method = method,
    bandwidth = band_width,
    position = position
  )

  df <- do.call(rbind.data.frame, lapply(df, as.data.frame))

  # color var must be in the same order as originally passed
  # or colors do not match
  if(!is.null(lvls)){
    df[[color]] <- factor(df[[color]], levels = lvls)
    df <- df[order(df[[color]]),]
  }

  fig_primitive(
    g, 
    ..., 
    data = df, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "line",
    asp = asp
  )
}

#' Density
#' 
#' Add a density figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @details Requires the `x` and `y` aspects.
#' 
#' @examples 
#' g2(cars, asp(speed, dist)) %>% 
#'  fig_density()
#' 
#' g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
#'  fig_density()
#' 
#' @importFrom purrr map
#' 
#' @export 
fig_density <- function(
  g, 
  ...,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_density")
}

#' @method fig_density g2r
#' @export 
fig_density.g2r <- function(
  g, 
  ...,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_density_(
    g, 
    ...,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_density g2Proxy
#' @export 
fig_density.g2Proxy <- function(
  g, 
  ...,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_density_(
    g, 
    ...,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Density
#' 
#' @inheritParams fig_density
#' 
#' @keywords internal
fig_density_ <- function(
  g, 
  ...,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
) {
  # aspects
  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")
  color <- select_asp_labels(asp, "color")

  if(length(position) < 2)
    stop("Must pass `x` and `y` aspects", call. = FALSE)

  # get data for split
  data <- get_data(g, data)

  if(length(color))
    data <- split(data, data[[color]])
  else
    data <- list(data)

  df <- map(data, function(df, pos, color){

    density <- stats::density(df[[pos[1]]])
    tidy <- data.frame(
      x = density$x,
      y = density$y
    )
    names(tidy) <- pos

    if(length(color))
      tidy[[color]] <- unique(df[[color]])

    return(tidy)
  }, pos = position, color = color)

  df <- do.call(rbind, lapply(df, as.data.frame))

  fig_primitive(
    g, 
    ..., 
    data = df, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "area",
    asp = asp
  )
}

#' Range
#' 
#' Add a range figure to the chart.
#' 
#' @inheritParams fig_point
#' @param type Type of figure to use
#' 
#' @details Requires the `ymin` and `ymax` aspects.
#' 
#' @examples 
#' df <- data.frame(
#'  x = 1:100,
#'  ymin = runif(100, 1, 5),
#'  ymax = runif(100, 6, 13)
#' )
#' 
#' g2(df, asp(x, ymin = ymin, ymax = ymax)) %>% 
#'  fig_range()
#' 
#' @export 
fig_range <- function(
  g, 
  ..., 
  type = c("interval", "area"),
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_range")
}

#' @method fig_range g2r
#' @export 
fig_range.g2r <- function(
  g, 
  ..., 
  type = c("interval", "area"),
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_range_(
    g, 
    ..., 
    type = type,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_range g2Proxy
#' @export 
fig_range.g2Proxy <- function(
  g, 
  ..., 
  type = c("interval", "area"),
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_range_(
    g, 
    ..., 
    type = type,
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Range
#' 
#' @inheritParams fig_range
#' 
#' @keywords internal
fig_range_ <- function(
  g, 
  ..., 
  type = c("interval", "area"),
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
) {
  type <- match.arg(type)

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  cols <- select_asp_labels(asp, "ymin", "ymax")

  if(length(cols) < 2)
    stop("Must pass `ymin` and `ymax` aspects", call. = FALSE)

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
    type = type,
    asp = asp
  )
}

#' Pie
#' 
#' Add a pie figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @examples 
#' df <- data.frame(
#'  label = letters[1:5],
#'  value = runif(5)
#' )
#' 
#' g2(df, asp(y = value, color = label)) %>% 
#'  fig_pie()
#' 
#' @export 
fig_pie <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_pie")
}

#' @method fig_pie g2r
#' @export 
fig_pie.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  g %>% 
    fig_interval(
      ..., 
      adjust("stack"),
      sync = sync,
      data = data,
      inherit_asp = inherit_asp
    ) %>% 
    coord_type("theta") %>% 
    gauge_x_linear(nice = FALSE)
}

#' Voronoi
#' 
#' Add a voronoi figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @details Requires the `x`, `y`, and `color` arguments.
#' 
#' @examples 
#' df <- data.frame(
#'  x = runif(25, 1, 500),
#'  y = runif(25, 1, 500),
#'  value = runif(25, 1, 500)
#' )
#' 
#' g2(df, asp(x, y, color = value)) %>% 
#'  fig_voronoi()
#' 
#' @export 
fig_voronoi <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_voronoi")
}

#' @method fig_voronoi g2r
#' @export 
fig_voronoi.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_voronoi_(
    g, 
    ..., 
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' @method fig_voronoi g2Proxy
#' @export 
fig_voronoi.g2Proxy <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  fig_voronoi_(
    g, 
    ..., 
    sync = sync, 
    data = data, 
    inherit_asp = inherit_asp
  )
}

#' Voronoi
#' 
#' @inheritParams fig_voronoi
#' 
#' @keywords internal
fig_voronoi_ <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
) {
  check_alter()

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")
  color <- select_asp_labels(asp, "color")
  data <- get_data(g, data)[c(position, color)]
  size <- c(max(data[[position[1]]]), max(data[[position[2]]]))

  if(ncol(data) < 3)
    stop("Must pass `x`, `y`, and `color` aspects", call. = FALSE)

  data <- alter::Alter$new(data)$
    source()$
    transform(
      type = "diagram.voronoi",
      fields = position,
      size = size,
      as = position
    )$
    getRows()

  fig_primitive(
    g, 
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "polygon",
    asp = asp
  )
}

#' Waffle
#' 
#' Add a waffle figure to the chart.
#' 
#' @inheritParams fig_point
#' @param n Number of squares to use.
#' @param rows Number of rows.
#' @param size Size of squares.
#' @param gap Gap between squares.
#' @param min_size Minimum size of squares.
#' 
#' @details Requires the `x` and `color` aspects.
#' 
#' @examples 
#' fruits <- data.frame(
#'  fruit = c("Apples", "Bananas", "Pears", "Oranges"),
#'  value = c(.45, .15, .35, .05) * 100
#' )
#' 
#' g2(fruits, asp(value, color = fruit)) %>% 
#'  fig_waffle() %>% 
#'  motif(padding = 50) %>% 
#'  axis_hide()
#' 
#' @export 
fig_waffle <- function(
  g, 
  ..., 
  n = 500,
  rows = 10,
  size = c(1, 1),
  gap = .1,
  min_size = 15,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_waffle")
}

#' @method fig_waffle g2r
#' @export 
fig_waffle.g2r <- function(
  g, 
  ..., 
  n = 500,
  rows = 10,
  size = c(1, 1),
  gap = .1,
  min_size = 15,
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){

  check_alter()

  data <- get_data(g, data)

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  color <- select_asp_labels(asp, "color")
  x <- select_asp_labels(asp, "x")
  fields <- c(color, x)

  if(length(fields) < 2)
    stop("Must pass `x`, and `color` aspects", call. = FALSE)

  data <- alter::Alter$new(data)$
    source()$
    transform(
      type = "waffle",
      maxCount = n,
      rows = rows,
      size = size,
      gapRatio = gap,
      as = c("waffle_x", "waffle_y"),
      fields = fields
    )$
    getRows()

  asp$x <- "waffle_x"
  asp$y <- "waffle_y"

  if(!"shape" %in% names(asp))
    asp$shape <- "square"

  if(!"size" %in% names(asp))
    asp$size <- "_hStep"

  cb <- sprintf(
    "function(_hStep) {
      return Math.min((window.innerHeight - 100) * 0.4 * _hStep, %s);
    }",
    min_size
  )

  g %>% 
    fig_primitive(
      ..., 
      data = data, 
      inherit_asp = inherit_asp,
      sync = sync,
      type = "point",
      asp = asp
    ) %>% 
    gauge_size(htmlwidgets::JS(cb)) %>% 
    legend_asps("_hStep", FALSE)
}

#' Rug
#' 
#' Add a rug figure to the chart.
#' 
#' @inheritParams fig_point
#' @param axis Axis to place the rug marks on.
#' @param strokeOpacity Opacity of rug marks.
#' 
#' @details Requires the `x` and `y` aspects.
#' 
#' @examples 
#' g2(mtcars, asp(wt, mpg)) %>% 
#'  fig_point() %>% 
#'  fig_rug() %>% 
#'  fig_rug(asp(size = 10), axis = "y")
#' 
#' @export 
fig_rug <- function(
  g, 
  ..., 
  strokeOpacity = .5,
  axis = c("x", "y"),
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_rug")
}

#' @method fig_rug g2r
#' @export 
fig_rug.g2r <- function(
  g, 
  ..., 
  strokeOpacity = .5,
  axis = c("x", "y"),
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){

  axis <- match.arg(axis)

  index <- 1
  if(axis == "y")
    index <- 2

  shape <- "hyphen"
  if(axis == "y")
    shape <- "line"

  # aspects
  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")
  color <- select_asp_labels(asp, "color")

  # get data for split
  data <- get_data(g, data)
  data[[position[index]]] <- 0

  if(is.null(asp$shape))
    asp$shape <- shape

  fig_primitive(
    g,
    ..., 
    strokeOpacity = .5,
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "point",
    asp = asp
  ) 
}

#' Candle
#' 
#' Add a candle figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @details Requires the following aspects defined:
#' 
#' - `open`
#' - `close`
#' - `high`
#' - `low`
#' 
#' If no `color` argument is passed the candles are colored 
#' according to their trend (open > close = "up").
#' 
#' @examples 
#' stock <- structure(list(date = structure(c(18626, 18627, 18631, 18632), class = "Date"), 
#'  open = c(39.52, 39.330002, 40.169998, 41.5), high = c(39.73, 
#'  40, 41.560001, 42.040001), low = c(39.200001, 39.029999, 
#'  39.939999, 40.77), close = c(39.34, 39.880001, 41.400002, 
#'  41.16)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", 
#'  "data.frame")
#' )
#' 
#' g2(stock, asp(date, open = open, close = close, high = high, low = low)) %>% 
#'  fig_candle() %>% 
#'  gauge_x_time_cat()
#' 
#' @export 
fig_candle <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_candle")
}

#' @method fig_candle g2r
#' @export 
fig_candle.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(
    asp,
    "open",
    "close",
    "high",
    "low"
  )

  if(length(position) != 4)
    stop(
      "Must pass `open`, `close`, `high`, and `low` aspects", 
      call. = FALSE
    )

  data <- get_data(g, data)

  # add range
  data$range <- pmap(data[, position], list) %>% 
    map(function(row,position){
      c(
        row[[position[4]]], 
        row[[position[1]]], 
        row[[position[2]]], 
        row[[position[3]]]
      )
    }, position = position)

  asp$y <- "range"
  asp$shape <- "candle"
  
  # add trend
  trend <- select_asp_labels(asp, "color")

  if(!length(trend)){
    data$trend <- ifelse(
      data[[position[4]]] < data[[position[1]]],
      "Up",
      "Down"
    )

    asp$color <- "trend"
  }

  fig_primitive(
    g,
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "schema",
    asp = asp
  ) 
}

#' Error
#' 
#' Add an error bar figure to the chart.
#' 
#' @inheritParams fig_point
#' 
#' @details Requires the `ymin` and `ymax` aspects, the
#' width of the error bars can be changed with the `size`
#' aspect.
#' 
#' @examples 
#' df <- data.frame(
#'  x = as.factor(c(1:10, 1:10)),
#'  y = runif(20, 10, 15),
#'  grp = rep(c("A", "B"), each = 2)
#' )
#' 
#' df$ymin <- df$y - runif(20, 1, 2)
#' df$ymax <- df$y + runif(20, 1, 2)
#' 
#' g2(df, asp(x = x, color = grp)) %>% 
#'  fig_error(asp(ymin = ymin, ymax = ymax), adjust("dodge")) %>% 
#'  fig_interval(
#'    asp(y = y), 
#'    adjust("dodge"),
#'    fillOpacity = .4
#'  )
#' 
#' @export 
fig_error <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){
  UseMethod("fig_error")
}

#' @method fig_error g2r
#' @export 
fig_error.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE
){

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "ymin", "ymax")

  if(length(position) != 2)
    stop("Must pass `ymin`, `ymax` aspects", call. = FALSE)

  data <- get_data(g, data)

  data$range <- pmap(data[, position], list) %>% 
    map(function(row,position){
      c(row[[position[2]]], row[[position[1]]])
    }, position = position)

  asp$y <- "range"
  asp$shape <- "tick"

  fig_primitive(
    g,
    ..., 
    data = data, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "interval",
    asp = asp
  ) 
}

#' Contour
#' 
#' Add a contour line figure to the chart.
#' 
#' @inheritParams fig_point
#' @param colors A palette of colors to define the `stroke`
#' of each path.
#' @param nlevels Passed to `contoureR::getContourLines`. 
#' An integer number of bins to split the data into *iff* 
#' ‘levels’ or ‘binwidth’ have not been specified.
#' @param binwidth Passed to `contoureR::getContourLines`.
#' The desired width of the bins, if specified, will override
#' ‘nlevels’
#' @param levels Passed to `contoureR::getContourLines`.
#' A numeric vector of the explicitly specified levels (zvalues) 
#' to contour, by specifying this argument, it will override 
#' ‘nlevels’ and/or ‘binwidth’. If this argument is provided, 
#' the stacking order of the contours will be preserved in the 
#' order of first occurence within the supplied vector.
#' @param criticalRatio Passed to `contoureR::getContourLines`.
#' When producing the Delaunay Mesh, the quality of the mesh can 
#' be poor in the proximity to the convex hull, Del's that have 
#' an aspect ratio greater than this value are not considered when 
#' producing the contours. In this context, the aspect ratio is 
#' defined as the circumradius to twice its inradius, equilateral 
#' triangles have an aspect ratio of 1, everything else is larger
#' 
#' @details Requires the `x`, `y` and `z` aspects, the
#' width of the error bars can be changed with the `size`
#' aspect.
#' 
#' @examples 
#' data(volcano)
#' 
#' x <- 1:nrow(volcano)
#' y <- 1:ncol(volcano)
#' df <- expand.grid(x = x,y = y)
#' df$z = apply(df, 1, function(x){ 
#'  volcano[x[1],x[2]]
#' })
#' 
#' g <- g2(df, asp(x, y, z = z)) 
#' 
#' fig_contour(g)
#' 
#' fig_contour(g, colors = c("red", "blue"))
#' 
#' @importFrom grDevices colorRampPalette
#' 
#' @export 
fig_contour <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  colors = NULL,
  nlevels = 10, 
  binwidth, 
  levels, 
  criticalRatio = 5
){
  UseMethod("fig_contour")
}

#' @method fig_contour g2r
#' @export 
fig_contour.g2r <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  colors = NULL,
  nlevels = 10, 
  binwidth, 
  levels, 
  criticalRatio = 5
){

  check_package("contoureR")

  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y", "z")

  if(length(position) != 3)
    stop("Must pass `x`, `y`, and `z` aspects", call. = FALSE)

  data <- get_data(g, data)

  data <- contoureR::getContourLines(
    data[[position[1]]], 
    data[[position[2]]],
    data[[position[3]]],
    nlevels = 10, 
    binwidth, 
    levels, 
    criticalRatio = 5
  )

  asp$x <- "x"
  asp$y <- "y"

  data_list <- split(data, data[["Group"]])

  if(!is.null(colors))
    colors <- colorRampPalette(colors)(length(data_list))

  for(i in 1:length(data_list)){
    col <- "#5B8FF9"
    if(!is.null(colors))
      col <- colors[i]
    
    g <- fig_primitive(
      g,
      ..., 
      stroke = col,
      data = data_list[[i]], 
      inherit_asp = inherit_asp,
      sync = sync,
      type = "path",
      asp = asp
    ) 
  }

  g
}

