#' @title Cut a numeric vector by quartiles of the vector.
#'
#' @description Cuts a numeric vector by quartiles of the vector.
#'
#' @param var A numeric vector.
#' @param labels If not NULL, these are the levels for the resulting factor.

#' @return A factor representing the categories after cutting the vector.
#' @examples
#' quartiles(iris$Sepal.Length,c('Low','Middle low','Middle high','High'))
#'
#' @export
#'

quartiles <- function(var){
  cut_quantiles(var, percentiles=c(0,0.25,0.5,0.75,1), labels=labels)
}
