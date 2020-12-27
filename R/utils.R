#' @importFrom purrr keep
drop_nulls <- function(x) {
  keep(x, function(x){
    length(x) > 0
  })
}

select_columns <- function(data = NULL, cols){
  if(is.null(data))
    return(NULL)
  data[, names(data) %in% cols]
}

#' Pass an action bound to an aspect
#' 
#' @section Uses:
#' 
#' - `legend_asps`
#' - `axis_asps`
#' 
#' @keywords internal
aspect_action <- function(g, asps, ..., action){
  if(missing(asps))
    stop("Missing `asps`", call. = FALSE)

  if(missing(action))
    stop("Missing `action`", call. = FALSE)

  asps <- unique(asps)

  item <- lapply(asps, function(c, opts){

    # can be chart.action(false)
    if(is.logical(opts[[1]]))
      opts <- opts[[1]]

    list(
      column = c,
      opts = opts
    )
  }, opts = list(...))

  g$x[[action]] <- append(g$x[[action]], item)

  g
}

# #' @importFrom purrr keep discard
# #' @importFrom rlang as_label is_quosure
# info_data_as_list <- function(..., data){

#   if(is.null(data))
#     return()

#   data <- tibble::tibble(data)

#   asp <- get_asp(...)
#   asp_keep <- keep(asp, is_quosure)
#   asp_keep_label <- sapply(asp_keep, as_label)
#   if(inherits(data, "data.frame")){
#     data <- data[, asp_keep_label]
#     names(data) <- names(asp_keep)
#   }

#   asp <- discard(asp, is_quosure)
#   if(length(asp)){
#     x <- as.character(asp)
#     names(x) <- names(asp)
#     data <- cbind.data.frame(data, as.data.frame(t(x)))
#   }

#   apply(data, 1, as.list)
# }
