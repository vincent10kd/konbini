#' @title Transform a continuous variable to approximately normal distribution
#'
#' @description Tranforms a continuous variable to be roughly normally distributed, picking the transformation that results in the lowest absolute skewness.
#'
#' @param x A vector.
#'
#' @return Returns the transformed vector.
#' @examples
#' hist(iris$Sepal.Length)
#' hist(transform_to_normal(iris$Sepal.Length))
#' hist(transform_to_normal(iris$Sepal.Length, rank=TRUE))
#' @export
#'

transform_to_normal <- function(x, constant = 1, rank=FALSE){
  identity <- x
  ln <- log(x + constant)
  sqrt <- sqrt(x)
  inverse <- -(1/x)
  if(rank){
      invnorm <- function(x){
      rank.x <- rank(x, na.last = "keep", ties.method = "random")
      N <- length(na.omit(x))
      qnorm((rank.x - (3/8)) / (N - 2 * (3/8) + 1))
    }
    invnorm <- invnorm(x)
    print('Inverse normal transformation applied')
    return(invnorm)
  }
  transformations <- data.frame(identity, ln, sqrt, inverse)
  trans_ind <- which.min(abs(sapply(transformations, konbini::skew)))
  print(colnames(transformations)[trans_ind])
  transformations[, trans_ind]
}

