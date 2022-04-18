#' @title Cut a numeric vector by specified percentiles
#'
#' @description Cuts a numeric vector by specified percentiles.
#'
#' @param var A numeric vector.
#' @param percentiles A vector of percentiles to cut by.
#' @param labels If not NULL, these are the levels for the resulting factor.

#' @return A factor representing the categories after cutting the vector.
#' @examples
#' cut_quantiles(iris$Sepal.Length, c(0,0.33,0.66,1),c('Low','Middle','High'))
#'
#' @export
#'

cut_quantiles <- function(var, percentiles, labels=NULL){
  newv <- cut(var, quantile(var, percentiles, na.rm=TRUE),include.lowest=TRUE)
  if(!is.null(labels)) levels(newv) <- labels
  newv
}
