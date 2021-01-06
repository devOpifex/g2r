#' Planes
#' 
#' Split the chart into planes according to variables.
#' 
#' @inheritParams fig_point
#' @param asp Aspects that define split, these must be defined
#' as a formula, e.g.: ~x+y.
#' @param ... Any other option.
#' @param type Type of planes to use.
#' @param sync Whether to sync the aspects used for the planes
#' with others used elsewhere, similar to that of [fig_point()].
#' 
#' @examples 
#' g2(iris, asp(Sepal.Length, Sepal.Width, color = Species)) %>% 
#'  fig_point() %>% 
#'  planes(~Species, type = "tree")
#' 
#' @importFrom rlang enquos as_label
#' @importFrom purrr map
#' @importFrom stats terms
#' 
#' @export 
planes <- function(
  g,
  asp,
  ...,
  type = c(
    "rect",
    "list",
    "matrix",
    "circle",
    "tree",
    "mirror"
  ),
  sync = TRUE
){
  UseMethod("planes")
}

#' @method planes g2r
#' @export 
planes.g2r <- function(
  g,
  asp,
  ...,
  type = c(
    "rect",
    "list",
    "matrix",
    "circle",
    "tree",
    "mirror"
  ),
  sync = TRUE
){
  if(missing(asp))
    stop("Missing `asp`", call. = FALSE)

  if(!inherits(asp, "formula"))
    stop("Must pass `asp` as formula, e.g.: ~x+y")
  
  if(is.null(g$x$data))
    stop("Planes requires data to be passed to `g2`", call. = FALSE)

  type <- match.arg(type)
  asp <- parse_form(asp)
  
  for(i in 1:length(asp)){
    g <- sync(g, asp[i], sync, if_true = "mainGroupPlanes")
  }

  g$x$cols <- c(g$x$cols, asp)

  g$x$facet <- list(
    type = type, 
    opts = list(
      fields = as.list(asp),
      ...
    )
  )

  g
}

#' Parse Planes Formula
#' 
#' @param frm Formula to parse.
#' 
#' @return A vector of variables used in the formula.
#' 
#' @keywords internal
parse_form <- function(frm) {
  vars <- as.list(attr(terms(frm), "variables"))[-1]

  vars %>% 
    map(as_label) %>% 
    unlist()
}
