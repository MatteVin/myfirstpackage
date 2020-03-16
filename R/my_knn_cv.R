#' k-Nearest Neighbors Cross-Validation
#'
#' This function is used to fit linear models. It can be used to carry out
#' regressions.
#'
#' @param train input data frame.
#' @param cl true class value of training data.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#' @keywords potato
#' @return A table containing all numerics, with one row for each coefficient
#' including the intercept and colums for the \code{Estimate}, the
#' \code{Std. Error}, the \code{t value}, and \code{Pr(>|t|)} (the p-value for
#' the t-test).
#' @import stats class dplyr magrittr
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  set.seed(302)
  n <- length(cl)
  fold <- sample(rep(1:k_cv, length = n))
  cross_val <- rep(NA, k_cv)
  # loop thru the folds
  for (i in 1:k_cv) {
    data_train <- train[fold != i, ] # Xi
    data_test <-  train[fold == i, ]  # Xi star
    # Train our models
    cl_train <- cl[fold != i] # Yi
    cl_test <- cl[fold == i]  # Yi star
    knn_output <- knn(train = data_train, cl = cl_train,
                      test = data_test, k = k_nn) #Yi star hat
    cross_val[i] <- sum(knn_output == cl_test) / length(cl_test)
  }
  yhat_star <- knn(train = train, cl = cl,
                   test = train, k = k_nn)
  output <- list("class" = yhat_star, "cv_error" = mean(cross_val))
}
