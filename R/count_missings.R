#' @title Count the number of missings in a data.frame or matrix
#'
#' @description Count the number of missings in a data.frame or matrix
#'
#' @param dat A data.frame or matrix

#' @return A data.frame with 3 columns: valid observations, missing observations, and percentage missing.
#' @examples
#' count_missings(iris)
#' iris2 <- as.matrix(iris)
#' iris2[sample(prod(dim(iris2)),20,replace=F)] <- NA
#' iris2 <- as.data.frame(iris2)
#' count_missings(iris2)
#'
#' @export
#'

count_missings <- function(dat){
  valid.n <- sapply(dat, function(x) sum(!is.na(x)))
  missing.n <- sapply(dat, function(x) sum(is.na(x)))
  percov <- round((valid.n/(valid.n + missing.n)) * 100, 2)
  return(data.frame(cbind(valid.n, missing.n, coverage = percov)))
}
