#' @title Predict difference in outcome for a pair of linear predictors
#'
#' @description Gives the difference in predicted y for two rows in X (X = x1 vs X = x2), based on a given regression model. Returns a confidence interval for the difference based on bootstrap samples. A ratio of predictions can also be returned.
#'
#' @param mod The regression model based on which to generate the predictions.
#' @param x1 One row in X to compare against another row in X.
#' @param x2 One row in X to compare against another row in X
#' @param type The type of prediction. By default the difference is on the scale of the linear predictor.
#' @param ratio If TRUE, returns the ratio of predictions instead of the difference.
#' @param ci If TRUE, returns a bootstrapped confidence interval (95%).
#' @param data If ci is TRUE, the data based on which the bootstrap samples are to be made (same as the data that informed the model) should be provided.
#' @param boot.iter Number of bootstrap samples.
#' @param decimals Number of decimals to display for the difference and its confidence interval.
#' @param plot If TRUE, plots the distribution of bootstrap differences based on which the confidence interval is created.
#' @param seed If not NULL, sets a random seed before bootstrapping.

#' @return NULL
#' @examples
#' predict_diff(lm(Sepal.Length ~., iris), x1 = iris[1,-1], x2 = iris[4,-1], ci = TRUE, data = iris)
#'
#' # what is the predicted difference in Sepal.Length between different species?
#'x0 <- iris[1,-1] # Species is setosa
#'x1 <- x0
#'x1$Species <- 'virginica'
#'x2 <- x0
#'x2$Species <- 'versicolor'
#'predict_diff(lm(Sepal.Length ~., iris), x1 = x1, x2 = x0, ci = TRUE, data=iris)
#'predict_diff(lm(Sepal.Length ~., iris), x1 = x1, x2 = x2, ci = TRUE, data=iris)
#'predict_diff(lm(Sepal.Length ~., iris), x1 = x2, x2 = x0, ci = TRUE, data=iris)
#'predict_diff(lm(Sepal.Length ~., iris), x1 = x2, x2 = x1, ci = TRUE, data=iris)
#'
#' @export
#'

predict_diff <- function(mod, x1, x2, type = NULL, ratio = FALSE,
                         ci = FALSE, data = NULL, boot.iter = 1e3,
                         decimals = 3, plot = TRUE, seed = NULL){

  pred1 <- predict(mod, newdata = x1, type = type)
  pred2 <- predict(mod, newdata = x2, type = type)
  diff <- pred1 - pred2
  if(ratio) diff <- pred1 / pred2

  if(ci){
    if(is.null(data)){
      stop('For a bootstrapped CI, the data argument should not be NULL')
    }
    if(!is.null(seed)) set.seed(seed)
    bdat <- konbini::bootstrap(data, rep = boot.iter)
    bres <- lapply(bdat, function(x){
      nmod <- update(mod, data = x)
      pred1 <- predict(nmod, newdata = x1)
      pred2 <- predict(nmod, newdata = x2)
      diff <- pred1 - pred2
      if(ratio) diff <- pred1 / pred2
      diff
    })
    bres <- do.call('c', bres)
    if(plot) plot(hist(bres, breaks = boot.iter / 4), main = 'Bootstrap differences', xlab='difference')
    cires <- round(quantile(bres, c(0.025,0.975)), decimals)
  }
  cat('The predicted difference in y for x1 and x2 is:\n', round(diff, decimals),
      ifelse(ci, paste0(' [',cires[1],'; ',cires[2],'].\n'),'.\n'), sep = '')
}
