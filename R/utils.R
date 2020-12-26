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
