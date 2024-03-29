% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_missings.R
\name{count_missings}
\alias{count_missings}
\title{Count the number of missings in a data.frame or matrix}
\usage{
count_missings(dat)
}
\arguments{
\item{dat}{A data.frame or matrix}
}
\value{
A data.frame with 3 columns: valid observations, missing observations, and percentage missing.
}
\description{
Count the number of missings in a data.frame or matrix
}
\examples{
count_missings(iris)
iris2 <- as.matrix(iris)
iris2[sample(prod(dim(iris2)),20,replace=F)] <- NA
iris2 <- as.data.frame(iris2)
count_missings(iris2)

}
