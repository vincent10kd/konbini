#' @title Permutation of a variable or several variables
#'
#' @description Permutes a vector, data.frame or matrix.
#'
#' @param dat A vector, data.frame or matrix to permute.

#' @return The permuted vector, data.frame or matrix.
#' @examples
#' iris$Sepal.Length
#' set.seed(123)
#' perm_sep <- permute(iris$Sepal.Length)
#' perm_sep
#' cor.test(iris$Sepal.Length, perm_sep)
#'
#' @export
#'

permute <- function(dat){
  shuffle <- function(x) x[sample(length(x), length(x), replace=F)]
  if(ncol(as.matrix(dat))==1){
    res <- shuffle(dat)
  }
  else res <- sapply(dat, shuffle)
  res
}
