% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_kmers.R
\name{count_kmers}
\alias{count_kmers}
\title{Count the frequency of specified k-mers in a string}
\usage{
count_kmers(x, k)
}
\arguments{
\item{x}{A character string.}
}
\value{
Returns a numeric vector of length u^k, where u is the number of unique characters appearing across k-mers.
}
\description{
Calculates the frequency of k-mers in a target character string.
}
\examples{
count_kmers('abcd', kmers = 4)
}
