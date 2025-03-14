#' @title Count the frequency of specified k-mers in a string
#'
#' @description Calculates the frequency of k-mers in a target character string.
#'
#' @param x A character string.
#'
#' @return Returns a numeric vector of length u^k, where u is the number of unique characters appearing across k-mers.
#' @examples
#' count_kmers('abcd', kmers = 4)
#' @export
#'

count_kmers <- function(x, k) {
  kmer <- kmers(x, k)
  kmer_length <- nchar(kmer[1])
  xspl <- strsplit(x, split = "")[[1]]
  n <- nchar(x)
  all_kmers <- unlist(lapply(1:(n - kmer_length + 1), function(i) paste(xspl[i:(i + kmer_length - 1)], collapse = "")))
  DC <- table(factor(all_kmers, levels = kmer)) / (n - kmer_length + 1)
  return(DC)
}




