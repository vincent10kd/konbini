#' @title Reorder a data.frame/matrix by a column.
#'
#' @description Reorders a data.frame or matrix by a specified column.
#'
#' @param dat A data.frame or matrix.
#' @param sort_by The column index or name to sort the data.frame by.
#' @param decreasing If TRUE, sorts in decreasing order.

#' @return The reordered data.frame or matrix.
#' @examples
#' head(iris)
#' head(sort_df(iris, 'Petal.Length'))
#'
#' @export
#'

sort_df <- function(dat, sort_by=NULL, decreasing=TRUE){
  dat[order(dat[,sort_by],decreasing=decreasing),]
}
