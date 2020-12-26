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

sync <- function(main, position, sync = TRUE){
  # it's FALSE
  if(is.logical(sync) && !isTRUE(sync))
    return()

  if(isTRUE(sync)){
    syncX <- "mainGroupX"
    syncY <- "mainGroupY"
  }
  
  # sync x
  main[[position[1]]][["sync"]] <- syncX
  main[[position[2]]][["sync"]] <- syncY

  main
}


