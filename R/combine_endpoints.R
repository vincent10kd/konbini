#' @title Combine two events and times for time-to-event data
#'
#' @description Combines two events with times for time-to-event data.
#'
#' @param dat A data.frame or matrix.
#' @param events The event columns (a numeric/character vector of length 2) to combine
#' @param times The time columns (a numeric/character vector of length 2) to combine.
#' @param add If TRUE, adds the truncated time and recoded event variables to the input data.frame.
#' @param add.names A character vector with the colnames for the columns added if add=TRUE.

#' @return
#' @export
#'

combine_endpoints <- function(dat, events, times, add=FALSE, add.names=NULL){
  e <- dat[,events]
  t <- dat[,times]
  if(ncol(e)>2) stop('combine_endpoints() only works with 2 endpoints.')
  trunc_t <- min(sapply(t,max,na.rm=TRUE))
  trunc1 <- truncate_time(dat, events[1], times[1], max_follow_up = trunc_t)
  trunc2 <- truncate_time(dat, events[2], times[2], max_follow_up = trunc_t)

  comb_e <- as.numeric(trunc1$trunc_event==1|trunc2$trunc_event==1)
  comb_t <- pmin(trunc1$trunc_time, trunc2$trunc_time)

  cond1 <- which(trunc1$trunc_event==1&trunc2$trunc_event==0)
  cond2 <- which(trunc1$trunc_event==0&trunc2$trunc_event==1)

  comb_t[cond1] <- trunc1$trunc_time[cond1]
  comb_t[cond2] <- trunc2$trunc_time[cond2]
  comb_df <- data.frame(comb_event=comb_e, comb_time=comb_t)
  if(add==FALSE) return(comb_df)
  if(add==TRUE){
    if(!is.null(add.names)){
      colnames(comb_df) <- add.names
    }
    comb_df <- cbind(dat,comb_df)
    return(comb_df)
  }
}
