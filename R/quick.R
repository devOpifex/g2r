#' Quick Plot
#' 
#' Draw a quick plot.
#' 
#' @param object An object containing data to plot,
#' often a model.
#' @param conf_level Confidence level.
#' @param intercept Whether to display the intercept.
#' @param ... Ignored
#' @param names Names of the models.
#' 
#' @export
qg2 <- function(
  object, 
  ...,
  conf_level = .95,
  intercept = FALSE,
  names = NULL
)
{
  UseMethod("qg2")
}

#' @export 
#' @method qg2 lm
qg2.lm <- function(
  object, 
  ...,
  conf_level = .95,
  intercept = FALSE
){
  check_package("broom")

  tidied <- broom::tidy(
    object, 
    conf.int = TRUE, 
    conf.level = conf_level
  )

  if(!intercept)
    tidied <- tidied[tidied$term != "(Intercept)",] 

  g2(tidied, asp(x = "term")) %>% 
    fig_point(asp(y = "estimate", shape = "circle")) %>% 
    fig_range(asp(ymin = "conf.low", ymax = "conf.high", size = 1)) %>% 
    coord_transpose()

}

#' @export 
#' @method qg2 biglm
qg2.biglm <- function(
  object, 
  ...,
  conf_level = .95,
  intercept = FALSE
){
  check_package("broom")

  tidied <- broom::tidy(
    object, 
    conf.int = TRUE, 
    conf.level = conf_level
  )

  if(!intercept)
    tidied <- tidied[tidied$term != "(Intercept)",] 

  g2(tidied, asp(x = "term")) %>% 
    fig_point(asp(y = "estimate", shape = "circle")) %>% 
    fig_range(asp(ymin = "conf.low", ymax = "conf.high", size = 1)) %>% 
    coord_transpose()

}

#' @export 
#' @method qg2 list
qg2.list <- function(
  object, 
  ...,
  conf_level = .95,
  intercept = FALSE,
  names = NULL
){

  check_package("broom")

  if(is.null(names))
    names <- paste("Model", seq_along(object)) 

  tidied <- purrr::map2_dfr(
    object, 
    names,
    function(x, y){
      tidied <- broom::tidy(
        x,
        conf.int = TRUE, 
        conf.level = conf_level
      )
      tidied$model <- y
      tidied
    }
  )

  if(!intercept)
    tidied <- tidied[tidied$term != "(Intercept)",] 

  g2(tidied, asp(x = "term", color = "model")) %>% 
    fig_point(
      asp(y = "estimate", shape = "circle"),
      adjust("dodge")
    ) %>% 
    fig_range(
      asp(ymin = "conf.low", ymax = "conf.high", size = 1),
      adjust("dodge")
    ) %>% 
    coord_transpose()

}

#' @method qg2 survfit
#' @export 
qg2.survfit <- function(object, ...){

  has_strata <- !is.null(object$strata)

  main_asp <- asp("time", shape = "vh")
  if(has_strata)
    main_asp <- asp("time", shape = "vh", color = "strata")

  g2(object, main_asp) %>% 
    fig_line(asp(y = "estimate")) %>% 
    fig_ribbon(asp(ymin = "conf.low", ymax = "conf.high")) %>% 
    fig_point(asp(y = "n.censor.y", shape = "circle"), stroke = "black")
}

#' @method qg2 acf
#' @export 
qg2.acf <- function(object, ...){
  g2(object) %>% 
    fig_interval(asp("lag", "acf", shape = "line"))
}

#' @method qg2 xts
#' @export
qg2.xts <- function(object, ...) {
  tidied <- to_g2r(object)

  n <- ncol(tidied)

  if(n == 2){
    names(tidied) <- c("x", "y")

  g2(tidied, asp("x", "y")) %>% 
    fig_line()
  } else {
    names(tidied)[1:5] <- c("x", "open", "high", "low", "close")

    g2(tidied, asp("x", open = "open", close = "close")) %>% 
      fig_candle(asp(high = "high", low = "low"))
  }

}

#' @method qg2 roc_df
#' @export 
qg2.roc_df <- function(object, ...){
  g2(object, asp("specificity", "sensitivity")) %>% 
    fig_line(asp(shape = "vh")) %>% 
    gauge_y_linear(min = 0, max = 1) %>% 
    coord_reflect("x") %>% 
    info_abline(
      style = list(
        lineDash = c(1,1)
      )
    )
}

#' @method qg2 matrix
#' @export 
qg2.matrix <- function(object, ...){
  g2(object) %>% 
    fig_polygon(asp("Var1", "Var2", color = "Freq"))
}

#' @method qg2 prcomp
#' @export 
qg2.prcomp <- function(object, ...){
  g2(object, asp("dim", "eig")) %>% 
    fig_interval() %>% 
    fig_line(stroke = "black") %>% 
    fig_point(fill = "black")
}

#' @method qg2 forecast
#' @export 
qg2.forecast <- function(object, ...){
  tidied <- to_g2r(object)

  base <- g2(tidied, asp("x")) %>% 
    fig_line(asp(y = "y")) %>% 
    fig_line(asp(y = "mean"))

  if(ncol(tidied) > 6){
    base %>% 
      fig_ribbon(asp(ymin = "lower_80", ymax = "upper_80")) %>% 
      fig_ribbon(asp(ymin = "lower_95", ymax = "upper_95"))
  } else {
    base %>% 
      fig_ribbon(asp(ymin = "lower_y", ymax = "upper_y"))
  }
}

#' @method qg2 igraph
#' @export 
qg2.igraph <- function(object, ...){
  g2(object, asp("x", "y")) %>%
    layout_igraph() %>% 
    fig_edge() %>% 
    fig_point(asp(shape = "circle")) %>% 
    axis_hide()
}

#' @method qg2 stl
#' @export 
qg2.stl <- function(object, ...){
  g2(object, asp("x", "value", color = "variable")) %>% 
    fig_line() %>% 
    planes(~variable, cols = 1, rows = 4, type = "list", padding = 20) %>% 
    motif(padding = c(10, 10, 30, 30))
}
