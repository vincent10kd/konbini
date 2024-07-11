#' @title Truncate survival times
#'
#' @description Truncates survival times and recodes the corresponding event.
#'
#' @param dat A data.frame or matrix.
#' @param event The event (a numeric/character vector of length 1) to recode.
#' @param time The time (a numeric/character vector of length 1) to truncate.
#' @param max_follow_up The follow-up time to truncate to.
#' @param add If TRUE, adds the truncated time and recoded event variable to the input data.frame.
#' @param add.names A character vector with the colnames for the columns added if add=TRUE.

#' @return
#' @export
#'

truncate_time <- function(dat, event, time, max_follow_up=1, add=FALSE, add.names=NULL){
  e <- dat[,event]
  t <- dat[,time]
  m <- max_follow_up
  e[which(t>m)] <- 0
  t[which(t>m)] <- m
  res <- data.frame(trunc_event=e,trunc_time=t)
  if(add==FALSE) return(res)
  if(add==TRUE){
    if(!is.null(add.names)){
      colnames(res) <- add.names
    }
    comb_df <- cbind(dat,res)
    return(comb_df)
  }
}
