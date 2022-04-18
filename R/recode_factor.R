#' @title Recode the levels of a factor
#'
#' @description Recodes a factor by relabeling it.
#'
#' @param var A factor.
#' @param new_levels The new levels of the factor, in order. The number of levels should be
#' equivalent to that of the original factor, but levels need not be unique.

#' @return The recoded factor.
#' @examples
#' table(iris$Species)
#' table(recode_factor(iris$Species, new_levels=c('Setosa','Not setosa','Not setosa')))
#'
#' @export
#'

recode_factor <- function(var,
                          new_levels=levels(var)){
  old_levels=levels(var)
  v <- factor(var, levels=old_levels, labels=new_levels)
  v
}
