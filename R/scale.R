#' Gauge Grid
#' 
#' Gauge the variables (`aspects`) used to define axis
#' and grid of the plot.
#' 
#' @inheritParams fig_point
#' @param ... Options to gauge variables 
#' (`aspects` defined by [asp()]).
#' @param asp Bare column name of aspect to apply the
#' gauge to.
#' @param nice Automatically adjust `min`, and `max`.
#' @param show_last Whether to force show the last tick
#' (only for `time` gauges).
#' @param range A vector of length 2 giving the minimum, and
#' maximum.
#' @param min,max Range of the gauge.
#' @param min_limit,max_limit Strict range of the ticks.
#' @param alias Alias name of the gauge and variable to 
#' display.
#' @param base Base of log.
#' @param tick_interval Minimum tick interval, only applies to 
#' linear type of gauge.
#' @param tick_count Maximum number of ticks.
#' @param max_tick_count Maximum number of ticks.
#' 
#' @section Types:
#' 
#' - `cat`: Categorical.
#' - `timeCat`: Categorical time.
#' - `linear`: Linear.
#' - `time`: Date, time, etc.
#' - `log`: Logarithmic.
#' - `pow`: Exponential.
#' - `quantize`: Manual quantiles.
#' - `quantile`: Auto-generated quantiles.
#' - `identity`: Constant.
#' 
#' @examples
#' g <- g2(cars, asp(speed, dist)) %>%
#'  fig_point()
#' 
#' g %>% gauge(speed, min = 0)
#' g %>% gauge_y_log(title = "Log")
#' g %>% gauge(dist, tickCount = 10)
#' 
#' @rdname gauge
#' @export 
gauge <- function(
  g, 
  asp, 
  ..., 
  nice = TRUE,
  range = NULL, 
  min = NULL,
  max = NULL,
  min_limit = NULL,
  max_limit = NULL,
  alias = NULL,
  tick_count = NULL,
  max_tick_count = NULL
){
  UseMethod("gauge")
}

#' @method gauge g2r
#' @export 
gauge.g2r <- function(
  g, 
  asp, 
  ..., 
  nice = TRUE,
  range = NULL, 
  min = NULL,
  max = NULL,
  min_limit = NULL,
  max_limit = NULL,
  alias = NULL,
  tick_count = NULL,
  max_tick_count = NULL
){

  col <- deparse(substitute(asp))

  gauge_(
    g, 
    col, 
    range = range, 
    min = min, 
    max = max,
    minLimit = min_limit,
    maxLimit = max_limit,
    alias = alias,
    tickCount = tick_count,
    maxTickCount = max_tick_count,
    ...
  )
}

#' @rdname gauge
#' @export 
gauge_x_time <- function(g, ..., show_last = FALSE){
  UseMethod("gauge_x_time")
}

#' @method gauge_x_time g2r
#' @export 
gauge_x_time.g2r <- function(g, ..., show_last = FALSE){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(
    g, 
    cols, 
    ..., 
    showLast = show_last, 
    type = "time"
  )
}

#' @rdname gauge
#' @export 
gauge_y_time <- function(g, ..., show_last = FALSE){
  UseMethod("gauge_y_time")
}

#' @method gauge_y_time g2r
#' @export 
gauge_y_time.g2r <- function(g, ..., show_last = FALSE){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(
    g, 
    cols, 
    ..., 
    showLast = show_last, 
    type = "time"
  )
}

#' @rdname gauge
#' @export 
gauge_x_linear <- function(g, ..., tick_interval = NULL){
  UseMethod("gauge_x_linear")
}

#' @method gauge_x_linear g2r
#' @export 
gauge_x_linear.g2r <- function(g, ..., tick_interval = NULL){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(
    g, 
    cols, 
    ...,
    tickInterval = tick_interval, 
    type = "linear"
  )
}

#' @rdname gauge
#' @export 
gauge_y_linear <- function(g, ..., tick_interval = NULL){
  UseMethod("gauge_y_linear")
}

#' @method gauge_y_linear g2r
#' @export 
gauge_y_linear.g2r <- function(g, ..., tick_interval = NULL){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(
    g, 
    cols, 
    ...,
    tickInterval = tick_interval, 
    type = "linear"
  )
}

#' @rdname gauge
#' @export 
gauge_x_cat <- function(g, ...){
  UseMethod("gauge_x_cat")
}

#' @method gauge_x_cat g2r
#' @export 
gauge_x_cat.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., type = "cat")
}

#' @rdname gauge
#' @export 
gauge_y_cat <- function(g, ...){
  UseMethod("gauge_y_cat")
}

#' @method gauge_y_cat g2r
#' @export 
gauge_y_cat.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., type = "cat")
}

#' @rdname gauge
#' @export 
gauge_x_time_cat <- function(g, ...){
  UseMethod("gauge_x_time_cat")
}

#' @method gauge_x_time_cat g2r
#' @export 
gauge_x_time_cat.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., type = "timeCat")
}

#' @rdname gauge
#' @export 
gauge_y_time_cat <- function(g, ...){
  UseMethod("gauge_y_time_cat")
}

#' @method gauge_y_time_cat g2r
#' @export 
gauge_y_time_cat.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., type = "timeCat")
}

#' @rdname gauge
#' @export 
gauge_x_log <- function(g, ..., base = 10){
  UseMethod("gauge_x_log")
}

#' @method gauge_x_log g2r
#' @export 
gauge_x_log.g2r <- function(g, ..., base = 10){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., base = base, type = "log")
}

#' @rdname gauge
#' @export 
gauge_y_log <- function(g, ..., base = 10){
  UseMethod("gauge_y_log")
}

#' @method gauge_y_log g2r
#' @export 
gauge_y_log.g2r <- function(g, ..., base = 10){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., base = base, type = "log")
}

#' @rdname gauge
#' @export 
gauge_x_pow <- function(g, ...){
  UseMethod("gauge_x_pow")
}

#' @method gauge_x_pow g2r
#' @export 
gauge_x_pow.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., type = "pow")
}

#' @rdname gauge
#' @export 
gauge_y_pow <- function(g, ...){
  UseMethod("gauge_y_pow")
}

#' @method gauge_y_pow g2r
#' @export 
gauge_y_pow.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., type = "pow")
}

#' @rdname gauge
#' @export 
gauge_x_quantile <- function(g, ...){
  UseMethod("gauge_x_quantile")
}

#' @method gauge_x_quantile g2r
#' @export 
gauge_x_quantile.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., type = "quantile")
}

#' @rdname gauge
#' @export 
gauge_y_quantile <- function(g, ...){
  UseMethod("gauge_y_quantile")
}

#' @method gauge_y_quantile g2r
#' @export 
gauge_y_quantile.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., type = "quantile")
}

#' @rdname gauge
#' @export 
gauge_x_quantize <- function(g, ...){
  UseMethod("gauge_x_quantize")
}

#' @method gauge_x_quantize g2r
#' @export 
gauge_x_quantize.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., type = "quantize")
}

#' @rdname gauge
#' @export 
gauge_y_quantize <- function(g, ...){
  UseMethod("gauge_y_quantize")
}

#' @method gauge_y_quantize g2r
#' @export 
gauge_y_quantize.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., type = "quantize")
}

#' @rdname gauge
#' @export 
gauge_x_identity <- function(g, ...){
  UseMethod("gauge_x_identity")
}

#' @method gauge_x_identity g2r
#' @export 
gauge_x_identity.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[1]
  gauge_(g, cols, ..., type = "identity")
}

#' @rdname gauge
#' @export 
gauge_y_identity <- function(g, ...){
  UseMethod("gauge_y_identity")
}

#' @method gauge_y_identity g2r
#' @export 
gauge_y_identity.g2r <- function(g, ...){
  cols <- get_aspect_names(g, "position")[2]
  gauge_(g, cols, ..., type = "identity")
}

#' Upsert Gauge
#' 
#' Upsert gauge for given column.
#' 
#' @inheritParams fig_point
#' @param cols Columns to apply the options to.
#' @param ... Options to pass to the gauge.
#' 
#' @keywords internal
gauge_ <- function(g, cols, ...){
  args <- list(...) %>% drop_nulls()
  for(col in cols){
    for(i in seq_along(args)){
      g$x$scale[[col]][[names(args)[i]]] <- args[[i]]
    }
  }
  g
}

#' Sync
#' 
#' Sets the syncs groups for x and y variables.
#' 
#' @inheritParams fig_point
#' @param asp Aspect to sync.
#' @param sync Either a logical or the name of a group.
#' 
#' @keywords internal
sync <- function(
  g, 
  asp, 
  sync = TRUE, 
  if_true = NULL
){
  
  # it's FALSE
  if(is.logical(sync) && !isTRUE(sync))
    return(g)

  if(!is.null(if_true))
    sync <- if_true
  
  # sync x
  g$x$scale[[asp]][["sync"]] <- sync

  g
}

#' Column to type
#' 
#' Get the type from a column, used for the 
#' gauge-family of functions.
#' 
#' @param data Data.frame or tibble.
#' @param col Column name.
#' 
#' @examples
#' \dontrun{
#' col_to_type(cars, "speed")
#' col_to_type(iris, "Species")
#' }
#' 
#' @importFrom utils head
#'  
#' @keywords internal
col_to_type <- function(col, data){
  x <- head(data[[col]])

  if(inherits(x, "POSIXct") || inherits(x, "POSIXlt"))
    return("time")

  if(inherits(x, "factor") || inherits(x, "character"))
    return("cat")

  "linear"
}

#' Set Gauges Types
#' 
#' @inheritParams fig_point
#' @param cols Column names to set.
#' @param data Dataset to use to determine types.
#' 
#' @keywords internal
gauges_types <- function(g, cols, data = NULL){
  if(is.null(data))
    data <- g$x$data

  types <- sapply(cols, col_to_type, data = data)

  for(i in seq_along(types)){
    if(!is.character(cols[i]))
      next

    print(cols[i])
    g <- gauge_(g, cols[i], type = types[i])
  }

  g
}
