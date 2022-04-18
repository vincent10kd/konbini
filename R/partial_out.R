#' @title Decorrelate a variable by one or several others using OLS
#'
#' @description Partials out variables by linear regression.
#'
#' @param dat A data.frame or matrix.
#' @param var The variable (a character vector of length 1) to be decorrelated.
#' @param decor_vars The variable(s) (character vector) to decorrelate by.

#' @return The decorrelated variable.
#' @examples
#' cor.test(iris$Petal.Length, iris$Sepal.Length)
#' decor_sepl <- partial_out(iris, 'Sepal.Length', 'Petal.Length')
#' cor.test(iris$Petal.Length, decor_sepl)
#'
#' @export
#'

partial_out <- function(dat, var, decor_vars){
  f <- reformulate(decor_vars,var)
  m <- lm(f, data=dat, na.action=na.exclude)
  res <- resid(m)
  res
}
