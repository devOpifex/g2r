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
    type = "polygon"
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
  inherit_asp = TRUE
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
  inherit_asp = TRUE
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
    coord_type("theta")
}

#' Voronoi
#' 
#' Add a voronoi figure to the chart.
#' 
#' @inheritParams fig_point
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

