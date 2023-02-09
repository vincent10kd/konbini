#' @title Exclude observations from a sample based on logical conditions
#'
#' @description Does the opposite of subset().
#'
#' @param dat A data.frame or matrix.
#' @param condition A logical condition specifying the exclusion criteria.
#' @param verbose If TRUE, prints how many rows were dropped based on the exclusion criteria.
#'
#' @return A subset of the original data.frame or matrix with the exclusion criteria applied.
#' @examples
#' exclude(iris, Species == 'setosa' | Sepal.Length > 6)
#' @export
#'

exclude <- function(dat, condition, verbose = TRUE){
  condition <- deparse(substitute(condition))
  dat2 <- as.data.frame(cbind(r_id = 1:nrow(dat), dat))
  sub <- eval(str2lang(paste0('subset(dat2,', condition,')')))
  dat2 <- subset(dat2, !r_id %in% sub$r_id)
  dat2$r_id <- NULL
  if(verbose) cat(nrow(dat)-nrow(dat2),'rows were excluded based on the exclusion criteria.\n')
  dat2
}

