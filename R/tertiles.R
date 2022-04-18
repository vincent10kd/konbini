#' @title Cut a numeric vector by tertiles of the vector.
#'
#' @description Cuts a numeric vector by tertiles of the vector.
#'
#' @param var A numeric vector.
#' @param labels If not NULL, these are the levels for the resulting factor.

#' @return A factor representing the categories after cutting the vector.
#' @examples
#' tertiles(iris$Sepal.Length,c('Low','Middle','High'))
#'
#' @export
#'

tertiles <- function(var, labels=NULL){
  cut_quantiles(var, percentiles=c(0,0.33,0.66,1), labels=labels)
}
