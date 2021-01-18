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
#' @examples 
#' \dontrun{to_g2r(AirPassengers)}
#' 
#' @export
to_g2r <- function(data = NULL) UseMethod("to_g2r")

#' @export
to_g2r.default <- function(data = NULL){
  as_tib(data)
}

#' @export
#' @method to_g2r survfit
to_g2r.survfit <- function(data = NULL){
  check_package("broom")
  tidied <- broom::tidy(data)
  tidied$n.censor.y <- ifelse(tidied$n.censor == 1, tidied$estimate, NA)
  tidied
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
#' @importFrom stats time
#' @method to_g2r mts
to_g2r.mts <- function(data = NULL) {
  check_package("zoo")
  
  base <- tibble(
    x = time(data) %>% zoo::as.Date()
  )

  df <- as_tib(data)

  cbind.data.frame(base, df)

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
#' @method to_g2r matrix
to_g2r.matrix <- function(data = NULL){
  as_tib(as.table(data))
}

#' @export
#' @method to_g2r xts
#' @importFrom tibble tibble
#' @importFrom stats time
to_g2r.xts <- function(data = NULL){
  x <- tibble(x = time(data))
  values <- as.data.frame(data)
  cbind.data.frame(x, values) %>% 
    as_tib()
}

#' @export
#' @method to_g2r forecast
to_g2r.forecast <- function(data = NULL) {
  x <- to_g2r(data$x)
  mean <- to_g2r(data$mean)
  lower <- to_g2r(data$lower)
  upper <- to_g2r(data$upper)
  fitted <- to_g2r(data$fitted)
  
  names(mean)[2] <- "mean"
  names(fitted)[2] <- "fitted"
  lower <- clean_forecast_names(lower, "lower_")
  upper <- clean_forecast_names(upper, "upper_")

  base <- merge(x, mean, by = "x", all = TRUE)
  bands <- merge(lower, upper, by = "x", all = TRUE)
  all <- merge(base, bands, by = "x", all = TRUE)
  merge(all, fitted, by = "x", all = TRUE)
}

clean_forecast_names <- function(mts, prefix = ""){
  nms <- names(mts)
  
  cleaned <- gsub("\\%", "", nms[2:length(nms)]) 
  cleaned <- paste0(prefix, cleaned)

  names(mts)[2:length(nms)] <- cleaned

  mts
}

#' @export 
#' @method to_g2r acf
to_g2r.acf <- function(data = NULL){
  check_package("broom")
  broom::tidy(data)
}

#' @export 
#' @method to_g2r loess
#' @importFrom stats predict
to_g2r.loess <- function(data = NULL){
  check_package("broom")
  augmented <- broom::augment(data)
  se <- unlist(predict(data, se = TRUE)[["se.fit"]])
  augmented[[".se"]] <- se
  augmented[[".lower"]] <- augmented[[".fitted"]] - augmented[[".se"]]
  augmented[[".upper"]] <- augmented[[".fitted"]] + augmented[[".se"]]
  augmented[order(unname(unlist(data$x[, 1]))),]
}

#' @export 
#' @method to_g2r lm
#' @importFrom stats predict
to_g2r.lm <- function(data = NULL){
  check_package("broom")
  augmented <- broom::augment(data)
  se <- unlist(predict(data, se = TRUE)[["se.fit"]])
  augmented[[".se"]] <- se
  augmented[[".lower"]] <- augmented[[".fitted"]] - augmented[[".se"]]
  augmented[[".upper"]] <- augmented[[".fitted"]] + augmented[[".se"]]
  augmented[order(unname(unlist(data$x[,1]))),]
}

#' @export 
#' @method to_g2r stl
#' @importFrom stats time
#' @importFrom tibble tibble
to_g2r.stl <- function(data = NULL){
  check_package("broom")

  ts <- data[["time.series"]]
  original <- ts %*% c(1,1,1)

  df <- to_g2r(ts)

  tibble(
    x = rep(df$x, 4),
    variable = rep(
      c("data", "seasonal", "trend", "remainder"),
      each = nrow(df)
    ),
    value = c(
      original,
      df$seasonal,
      df$trend,
      df$remainder
    )
  )
}

#' @export 
#' @method to_g2r prcomp
to_g2r.prcomp <- function(data, ...){
  eig <- data$sdev ^ 2 
  tibble(dim = factor(1:length(eig)), eig = eig)
}

#' @export 
#' @method to_g2r conf_mat
to_g2r.conf_mat <- function(data, ...){
  as.data.frame(data[["table"]])
}
