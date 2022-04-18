#' @title Perform a function in a nested for loop
#'
#' @description Perform a function within a nested for loop.
#'
#' @param outer Argument denoting what to iterate over for the outer loop.
#' @param inner Argument denoting what to iterate over for the inner loop.
#' @param fun An anonymous function with 2 arguments.
#' @param progress If TRUE, prints the iteration of all iterations outer * inner.

#' @return A list containing the output of the nested for loop.
#' @examples
#' matrix_do(outer=c(1,2,3),inner=letters[1:5], fun=function(x,y) print(paste0(x,y)))
#'
#' @export
#'

matrix_do <- function(outer=NULL,
                      inner=NULL,
                      fun=NULL,
                      progress=TRUE){
  resList <- list()
  out_l <- length(outer)
  in_l <- length(inner)
  for(i in 1:out_l){
    for(j in 1:in_l){
      counter <- j+in_l*(i-1)
      total <- out_l*in_l
      if(progress==TRUE) cat('Iteration ',counter,' of ',total,' (',round((counter/total)*100,2),'%)\n',sep='')
      resList[[counter]] <- fun(outer[[i]],inner[[j]])
    }
  }
  invisible(resList)
}
