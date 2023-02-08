#' @title Interleave two data.frames or matrices row by row
#'
#' @description Interleave two data.frames or matrices of equal dimensions, row by row.
#'
#' @param df1 Input data.frame or matrix.
#' @param df2 A second data.frame or matrix of equal dimensions as df1.
#' @param add_df_id If TRUE, adds an additional column indicating the original data.frame.

#' @return A combined data.frame.
#' @examples
#' iris_s1 <- iris[1:5, 1:4]
#' iris_s2 <- iris[1:5, 1:4] - 0.5
#' intersperse(iris_s1, iris_s2)
#'
#' @export
#'

intersperse <- function(df1, df2, add_df_id = TRUE){
  if(is.null(dim(df1))|is.null(dim(df2))){
    df1 <- as.matrix(df1)
    df2 <- as.matrix(df2)
  }
  if(any(dim(df1)!=dim(df2))) stop('Dimensions of both matrices/data.frames should be equivalent!')
  df3 <- as.data.frame(matrix(NA,nrow=nrow(df1)*2, ncol=ncol(df1)))
  for(i in 1:nrow(df1)){
    df3[seq(1,nrow(df3),2)[i],] <- df1[i,]
    df3[seq(2,nrow(df3),2)[i],] <- df2[i,]
  }
  colnames(df3) <- colnames(df1)
  if(add_df_id) df3$df_id <- rep(c(1,2),nrow(df1))
  return(df3)
}
