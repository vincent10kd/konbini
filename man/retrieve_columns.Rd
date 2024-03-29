% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve_columns.R
\name{retrieve_columns}
\alias{retrieve_columns}
\title{Find a column by regular expression}
\usage{
retrieve_columns(dat, query)
}
\arguments{
\item{dat}{A data.frame or matrix.}

\item{query}{A regular expression.}
}
\value{
A named vector of column indices matching the regular expression.
}
\description{
Retrieves the index and name of one or several variables by a regular expression.
}
\examples{
retrieve_columns(iris, 'Spec')
retrieve_columns(iris, 'Petal')

}
