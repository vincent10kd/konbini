#' @title Convert all factors in a data.frame to character
#'
#' @description Converts all factors in a data.frame to character.
#'
#' @param dat A data.frame

#' @return The input data.frame with all factors converted to character
#' @examples
#' sapply(iris, class)
#' iris2 <- all_factors_to_char(iris)
#' sapply(iris2, class)
#'
#' @export
#'

all_factors_to_char <- function(dat){
  cls <- sapply(dat, class)
  ind <- which(cls=='factor')
  dat[,ind] <- sapply(dat[,ind], as.character)
  dat
}
