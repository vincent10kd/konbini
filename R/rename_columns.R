#' @title Rename one or several columns
#'
#' @description Renames one or multiple columns in a data.frame or matrix.
#'
#' @param dat Input data.frame or matrix.
#' @param column The name(s) of the column(s) to be renamed
#' @param to The new name(s).

#' @return A data.frame with the renamed column(s)
#' @examples
#' head(iris)
#' head(rename_column(iris, 'Species', 'Renamed'))
#'
#' @export
#'

rename_columns <- function(dat, column, to){
  colnames(dat)[sapply(column, function(x) which(colnames(dat) == x))] <- to
  dat
}
