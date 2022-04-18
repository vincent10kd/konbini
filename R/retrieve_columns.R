#' @title Find a column by regular expression
#'
#' @description Retrieves the index and name of one or several variables by a regular expression.
#'
#' @param dat A data.frame or matrix.
#' @param query A regular expression.

#' @return A named vector of column indices matching the regular expression.
#' @examples
#' retrieve_columns(iris, 'Spec')
#' retrieve_columns(iris, 'Petal')
#'
#' @export
#'

retrieve_columns <- function(dat, query){
  ind <- grep(query, colnames(dat))
  names(ind) <- colnames(dat)[ind]
  ind
}
