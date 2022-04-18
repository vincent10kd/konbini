#' @title Winsorizing outliers
#'
#' @description Winsorizes univariate outliers, defined by a specified number of median absolute deviations from the median.
#'
#' @param dat A numeric vector.
#' @param mads The number of MADs away from the median defining a univariate outlier.

#' @return The winsorized vector.
#' @examples
#' set.seed(123)
#' x <- rnorm(1000)
#' x <- c(x, 5.1, 6.2, 8.9, 10.1, 12.2, -14, -18.4, -7.2) # add outliers
#' hist(x)
#' hist(winsorize(x)) # after winsorizing outliers
#'
#' @export
#'

winsorize <- function(dat, mads=4){

  winsor <- function(x){
    mad <- mad(x, na.rm=TRUE)
    med <- median(x, na.rm=TRUE)
    upper <- which(x>(med+mads*mad))
    lower <- which(x<(med-mads*mad))
    mdis <- mean(diff(sort(na.omit(x)),1))
    x[upper] <- med+mads*mad+mdis*order(x[upper])
    x[lower] <- med-mads*mad-mdis*order(x[lower])
    x
  }
  if(ncol(as.matrix(dat))==1){
    res <- winsor(dat)
  }
  else res <- sapply(dat, winsor)
  res
}
