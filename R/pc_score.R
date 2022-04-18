#' @title Create principal component scores
#'
#' @description Performs PCA on a data.frame/matrix and returns the PC scores.
#'
#' @param dat A data.frame or matrix.
#' @param vars The variables (numeric or character vector) to run PCA on.
#' @param no_of_comps The number of PC scores to return (default is 1).
#' @param scale If TRUE, centers and standardizes the matrix first.

#' @return The PC score(s).
#' @examples
#' head(pc_score(iris, c('Petal.Length','Sepal.Length','Sepal.Width')))
#' head(pc_score(iris, c('Petal.Length','Sepal.Length','Sepal.Width'),2))
#'
#' @export
#'

pc_score <- function(dat, vars, no_of_comps=1, scale=TRUE){
  X <- as.matrix(dat[,vars])
  if(scale==TRUE) X <- scale(X)
  covX <- cov(X, use='complete')
  eig <- eigen(covX)
  eigenval <- eig$values
  var_exp <- eigenval/sum(eigenval)
  cum_vexp <- cumsum(var_exp)
  cat('Principal component(s) 1 thru ',no_of_comps,' explain ',
      round(cum_vexp[no_of_comps]*100,2),'% of the variance.\n',sep='')
  score <- X%*%eig$vectors[,1:no_of_comps]
  score
}
