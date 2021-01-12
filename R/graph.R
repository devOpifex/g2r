#' Layout with igraph
#' 
#' Layout the graph using an igraph layout function.
#' This function only works with the graph was initialised
#' with an object of class `igraph`.
#' 
#' @inheritParams fig_point
#' @param ... Any options to pass to the `method` function.
#' @param method An igraph layout function to compute the
#' nodes and edges (source and target) position on the canvas.
#' 
#' @details The function runs the `method` to obtain the `x`
#' and `y` coordinates. These are added to the `nodes` 
#' data.frame (extracted from initial graph) and to the edges
#' data.frame, as `x` and `y` _nested_ columns, e.g.: 
#' `c(source_x, target_x)`. These `x` and `y` coordinates 
#' can be used in [asp()] (see example).
#' 
#' @examples 
#' ig <- igraph::make_ring(100)
#' 
#' # use x and y for positioning
#' g2(ig, asp(x, y)) %>%
#'  layout_igraph() %>% 
#'  fig_edge() %>% 
#'  fig_point(asp(shape = "circle")) %>% 
#'  axis_hide()
#' 
#' @importFrom purrr map
#' 
#' @export 
layout_igraph <- function(
  g, 
  ..., 
  method = igraph::layout_nicely
){
  check_package("igraph")
  UseMethod("layout_igraph")
}

#' @method layout_igraph g2r
#' @export 
layout_igraph.g2r <- function(
  g, 
  ..., 
  method = igraph::layout_nicely
) {

  if(is.null(g$x$graph))
    stop("Missing graph", call. = FALSE)

  # compute layout
  l <- g$x$graph %>% 
    method(...) %>% 
    as.data.frame()
  
  names(l) <- c("x", "y")

  # remove x and y then add 
  g$x$data$nodes[["x"]] <- g$x$data$nodes[["y"]] <- NULL
  g$x$data$nodes <- cbind.data.frame(g$x$data$nodes, l)

  # add source target
  tmp <- g$x$data$nodes[, c("id", "x", "y")]

  names(tmp) <- c("source", "source_x", "source_y")
  g$x$data$edges <- merge(
    x = g$x$data$edges, 
    y = tmp, 
    by = "source", 
    all.x = TRUE
  )

  names(tmp) <- c("target", "target_x", "target_y")
  g$x$data$edges <- merge(
    x = g$x$data$edges, 
    y = tmp, 
    by = "target", 
    all.x = TRUE
  )

  g$x$data$edges <- pmap(g$x$data$edges, list) %>% 
    map(function(edge){
      edge$x <- c(edge$source_x, edge$target_x)
      edge$y <- c(edge$source_y, edge$target_y)

      edge$source_x <- edge$target_x <- NULL
      edge$source_y <- edge$target_y <- NULL

      return(edge)
    })

  g
}

#' Layout Arc
#' 
#' Layout as arc using the alter package.
#' 
#' @inheritParams fig_point
#' @param sourceWeight,targetWeight Bare name of column containing
#' weights of source and target in the edges data.frame.
#' @param thickness Node height, between `0` and `1`.
#' @param marginRatio Space ratio, between `0` and `1`.
#' 
#' @examples 
#' ig <- igraph::erdos.renyi.game(100, 1/100)
#' 
#' g2(ig, asp(x, y)) %>% 
#'  layout_arc() %>% 
#'  fig_edge(asp(color = source, shape = "arc"), opacity = .3) %>% 
#'  fig_point(asp(color = id, shape = "circle", size = value)) %>% 
#'  coord_type("polar") %>% 
#'  coord_reflect("y") %>% 
#'  axis_hide()
#' 
#' g2(ig, asp(x, y)) %>% 
#'  layout_arc() %>% 
#'  fig_edge(asp(color = source, shape = "arc"), opacity = .3) %>% 
#'  fig_point(asp(color = id, shape = "circle", size = value))
#' 
#' @export 
layout_arc <- function(
  g,
  sourceWeight = NULL,
  targetWeight = NULL,
  thickness = .05,
  marginRatio = .1
){
  check_alter()
  UseMethod("layout_arc")
}

#' @method layout_arc g2r
#' @export 
layout_arc.g2r <- function(
  g,
  sourceWeight = NULL,
  targetWeight = NULL,
  thickness = .05,
  marginRatio = .1
) {

  src_wght_enquo <- enquo(sourceWeight)
  tgt_wght_enquo <- enquo(targetWeight)
  weight <- process_weight(g, src_wght_enquo, tgt_wght_enquo)

  alt <- alter::Alter$new(g$x$data)$
    source(
      type = "graph"
    )$
    transform(
      type = "diagram.arc",
      y = 0,
      weight = weight,
      thickness = thickness,
      marginRatio = marginRatio
    )

  g$x$data$edges <- alt$get("edges", clean = FALSE)
  g$x$data$nodes <- alt$get("nodes")

  g
}

# #' @export 
# layout_sankey <- function(
#   g,
#   nodeAlign = c(
#     "sankeyJustify",
#     "sankeyLeft",
#     "sankeyRight",
#     "sankeyCenter"
#   ),
#   nodeWidth = .2,
#   nodePadding = .02
# ){
#   check_alter()
#   UseMethod("layout_sankey")
# }

# #' @method layout_sankey g2r
# #' @export 
# layout_sankey.g2r <- function(
#   g,
#   nodeAlign = c(
#     "sankeyJustify",
#     "sankeyLeft",
#     "sankeyRight",
#     "sankeyCenter"
#   ),
#   nodeWidth = .2,
#   nodePadding = .02
# ){

#   node_align <- match.arg(nodeAlign)

#   alt <- alter::Alter$new(g$x$data)$
#     source(
#       type = "graph"
#     )$
#     transform(
#       type = "diagram.sankey",
#       nodeAlign = nodeAlign,
#       nodeWidth = nodeWidth,
#       nodePadding = nodePadding
#     )

#   g$x$data$edges <- alt$get("edges", clean = FALSE)
#   g$x$data$nodes <- alt$get("nodes")

#   g
# }

#' Process Weight
#' 
#' Process weights for [layout_arc()].
#' 
#' @inheritParams fig_point
#' @param src_wght_enquo,tgt_wght_enquo Source and target
#' columns asq quosures.
#' 
#' @importFrom rlang quo_is_null enquo as_label
#' 
#' @keywords internal
process_weight <- function(g, src_wght_enquo, tgt_wght_enquo){
  if(quo_is_null(src_wght_enquo) || quo_is_null(tgt_wght_enquo))
    return(FALSE)

  # source weight
  source_col <- as_label(src_wght_enquo)
  index <- c(1:ncol(g$x$data$edges))[names(g$x$data$edges) %in% source_col]
  names(g$x$data$edges)[index] <- "value"

  # target weight
  weight_col <- as_label(tgt_wght_enquo)
  index <- c(1:ncol(g$x$data$edges))[names(g$x$data$edges) %in% weight_col]
  names(g$x$data$edges)[index] <- "value1"

  g$x$cols <- c(g$x$cols, "value", "value1")

  TRUE
}