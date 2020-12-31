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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
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
    style = style,
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){

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

  # get data for split
  data <- get_data(g, data)

  if(length(color))
    data <- split(data, data[[color]])
  else
    data <- list(data)

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

  fig_primitive(
    g, 
    ..., 
    data = df, 
    inherit_asp = inherit_asp,
    sync = sync,
    type = "line",
    style = style,
    asp = asp
  )
}

#' Density
#' 
#' Add a density figure to the chart.
#' 
#' @inheritParams fig_point
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){

  # aspects
  asp <- get_combined_asp(g, ..., inherit_asp = inherit_asp)
  position <- select_asp_labels(asp, "x", "y")
  color <- select_asp_labels(asp, "color")

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
    style = style,
    asp = asp
  )
}

#' Range
#' 
#' Add a range figure to the chart.
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
#'  fig_range()
#' 
#' @export 
fig_range <- function(
  g, 
  ..., 
  sync = TRUE, 
  data = NULL, 
  inherit_asp = TRUE,
  style = NULL
){
  UseMethod("fig_range")
}

#' @method fig_range g2r
#' @export 
fig_range.g2r <- function(
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
    type = "interval",
    style = style,
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
  inherit_asp = TRUE,
  style = NULL
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
  inherit_asp = TRUE,
  style = NULL
){
  g %>% 
    fig_interval(..., adjust("stack")) %>% 
    coord_type("theta")
}
