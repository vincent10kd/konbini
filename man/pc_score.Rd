% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pc_score.R
\name{pc_score}
\alias{pc_score}
\title{Create principal component scores}
\usage{
pc_score(dat, vars, no_of_comps = 1, scale = TRUE)
}
\arguments{
\item{dat}{A data.frame or matrix.}

\item{vars}{The variables (numeric or character vector) to run PCA on.}

\item{no_of_comps}{The number of PC scores to return (default is 1).}

\item{scale}{If TRUE, centers and standardizes the matrix first.}
}
\value{
The PC score(s).
}
\description{
Performs PCA on a data.frame/matrix and returns the PC scores.
}
\examples{
head(pc_score(iris, c('Petal.Length','Sepal.Length','Sepal.Width')))
head(pc_score(iris, c('Petal.Length','Sepal.Length','Sepal.Width'),2))

}
