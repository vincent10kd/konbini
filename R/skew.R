#' @title Calculate skewness of a vector
#'
#' @description Calculates the skewness of a continuous variable
#'
#' @param x A vector.
#'
#' @return Returns the skewness of the input vector.
#' @examples
#' skew(iris$Sepal.Length)
#' @export
#'

skew <- function(x){
  (sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)
}

