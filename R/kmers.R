#' @title Generate k-mers from a string
#'
#' @description Retrieves k-mers from a character string.
#'
#' @param x A character string.
#'
#' @return Returns a character vector of length u^k, where u is the number of unique characters appearing across k-mers.
#' @examples
#' kmers('abcd', kmers = 4)
#' @export
#'

kmers <- function(x, k = 3){
  x <- unique(unlist(strsplit(x, '')))
  lz <- list()
  for(i in 1:k){
    lz[[i]] <- x
  }
  do.call('paste0', (do.call('expand.grid', list(lz))))
}


