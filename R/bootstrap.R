#' @title Create a bootstrap sample or return a bootstrapped statistic
#'
#' @description Create a bootstrap sample or return a bootstrapped statistic
#'
#' @param dat A data.frame or matrix.
#' @param n The sample size of the bootstrap sample. Same size as the original sample by default.
#' @param rep The number of times a bootstrap sample should be taken. If >1, returns a list.
#' @param fun An expression as a character string, to be evaluated internally. The data should be assumed attached.

#' @return A bootstrapped data.frame/matrix, or a numeric vector with a bootstrapped statistic.
#' @examples
#' # bootstrapped coefficient from a regression model
#' bootstrap(iris, rep = 100, fun = 'coef(lm(Sepal.Length ~ Species))[2]')
#'
#' # bootstrapped mean
#' bootstrap(iris, rep = 100, fun = 'mean(Petal.Length)')
#'
#' # bootstrap sample
#' iris_boot <- bootstrap(iris)
#' @export
#'

bootstrap <- function(dat, n = nrow(dat), rep = 1, fun = NULL){
  boots <- function(x) x[sample(1:nrow(x), n, replace=TRUE), ]
  if(rep < 1){
    stop('rep should be at least 1.')
  }
  if(rep == 1){
    dat <- list(boots(dat))
  }
  else{
    dat <- lapply(1:rep, function(x) boots(dat))
  }
  if(!is.null(fun)){
    res <- lapply(1:length(dat), function(z){
      fun <- paste0('with(dat[[z]],',fun,')')
      eval(str2lang(fun))
      }
    )
    res <- do.call('c', res)
    print(summary(res))
    cat('> An invisible vector of bootstrapped statistics was returned.\n')
    return(invisible(res))
  }
  else return(dat)
}
