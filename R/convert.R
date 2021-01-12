#' Convert to Tibble
#' 
#' Converts objects to objects g2r can work with, 
#' generally a `tibble::tibble`.
#' 
#' @param data An object to convert.
#' 
#' @details This is exposed so the user can understand
#' what happens under the hood and which variables/columns
#' can subsequently be used in figures with [asp()].
#' 
#' These methods are used in the [g2()] function to preprocess
#' `data` objects.
#' 
#' @export
to_g2r <- function(data = NULL) UseMethod("to_g2r")

#' @export
to_g2r.default <- function(data = NULL){
  as_tib(data)
}

#' @export
#' @importFrom stats time
#' @method to_g2r ts
to_g2r.ts <- function(data = NULL) {
  check_package("zoo")
  tibble(
    x = time(data) %>% zoo::as.Date(),
    y = as.vector(data)
  )
}

#' @export
#' @method to_g2r data.frame
to_g2r.data.frame <- function(data = NULL) {
  as_tib(data)
}

#' @export
#' @method to_g2r igraph
to_g2r.igraph <- function(data = NULL){
  check_package("igraph")

  # nodes and edges as data frame
  edges <- igraph::as_data_frame(data, what = "edges")
  nodes <- igraph::as_data_frame(data, what = "vertices")

  # rename for alter transform default
  names(edges)[1:2] <- c("source", "target")

  if(ncol(nodes) == 0)
    nodes <- data.frame(id = as.vector(igraph::V(data)))

  names(nodes)[1] <- "id"

  list(nodes = nodes, edges = edges)
}

#' @export
#' @method to_g2r forecast
to_g2r.forecast <- function(data = NULL) {
  x <- to_g2r(data$x)
  mean <- to_g2r(data$mean)
  lower <- to_g2r(data$lower)
  upper <- to_g2r(data$upper)
  
  names(mean) <- c("x", "mean")
  names(lower) <- c("x", "lower")
  names(upper) <- c("x", "upper")

  base <- merge(x, mean, by = "x", all = TRUE)
  bands <- merge(lower, upper, by = "x", all = TRUE)
  all <- merge(base, bands, by = "x", all = TRUE)
  
}
