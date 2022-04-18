#' @title Convert all factors in a data.frame to numeric
#'
#' @description Converts all factors in a data.frame to numeric.
#' This function is to be used when all factors have levels representing numeric values.
#' Otherwise, the function returns missings.
#'
#' @param dat A data.frame

#' @return The input data.frame with all factors converted to numeric
#' @examples
#' sapply(iris, class)
#' iris2 <- all_factors_to_char(iris)
#' sapply(iris2, class)
#'
#' @export
#'

all_factors_to_num <- function(dat){
  cls <- sapply(dat, class)
  ind <- which(cls=='factor')
  dat[,ind] <- sapply(dat[,ind], function(x) as.numeric(as.character(x)))
  dat
}
