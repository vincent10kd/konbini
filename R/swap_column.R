#' @title Swap two columns in a data.frame or matrix
#'
#' @description Swaps two columns in a data.frame or matrix.
#'
#' @param dat A data.frame or matrix.
#' @param column A column, by name or by index, to be swapped in position with another.
#' @param to Index for the second column, which will be swapped with the column named in 'column'.

#' @return A data.frame with the indicated columns swapped.
#' @examples
#' head(iris)
#' head(swap_column(iris, 5, 1))
#' head(swap_column(iris, 'Petal.Length', 2))
#'
#' @export
#'

swap_column <- function(dat, column, to){
  if('matrix'%in%class(dat)) dat <- as.data.frame(dat)
  if(class(column)=='character') from <- which(colnames(dat)==column)
  if(class(column)%in%c('numeric','integer')) from <- column
  old_seq <- 1:ncol(dat)
  new_seq <- replace(old_seq, c(to, from), c(from, to))
  dat[new_seq]
}
