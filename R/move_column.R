#' @title Move a column to a new position
#'
#' @description Moves a column to a designated position.
#'
#' @param dat Input data.frame or matrix.
#' @param column The name or index of the column to be moved.
#' @param to The column index to which the column should be moved.

#' @return A data.frame with the column in the new position.
#' @examples
#' head(iris)
#' head(move_column(iris, 5, 1))
#' head(move_column(iris, 'Petal.Length', 5))
#'
#' @export
#'

move_column <- function(dat, column, to){
  if('matrix'%in%class(dat)) dat <- as.data.frame(dat)
  if(class(column)=='character') from <- which(colnames(dat)==column)
  if(class(column)%in%c('numeric','integer')) from <- column
  old_seq <- 1:ncol(dat)
  if(to < from){
    if(to > 1){
      start_seq <- 1:(to-1)
      end_seq <- setdiff(old_seq,c(start_seq,from))
      new_seq <- c(start_seq,from,end_seq)}
    if(to==1){
      start_seq <- 1
      end_seq <- setdiff(old_seq,c(start_seq,from))
      new_seq <- c(from, start_seq, end_seq)
    }
  }
  if(to > from){
    start_seq <- setdiff(1:to,from)
    end_seq <- setdiff(old_seq,c(start_seq,from))
    new_seq <- c(start_seq,from,end_seq)
  }
  dat[new_seq]
}
