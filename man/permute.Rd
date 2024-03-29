% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permute.R
\name{permute}
\alias{permute}
\title{Permutation of a variable or several variables}
\usage{
permute(dat)
}
\arguments{
\item{dat}{A vector, data.frame or matrix to permute.}
}
\value{
The permuted vector, data.frame or matrix.
}
\description{
Permutes a vector, data.frame or matrix.
}
\examples{
iris$Sepal.Length
set.seed(123)
perm_sep <- permute(iris$Sepal.Length)
perm_sep
cor.test(iris$Sepal.Length, perm_sep)

}
