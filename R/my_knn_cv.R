#' k-Nearest Neighbors Cross-Validation.
#' @import class stats dplyr
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # check if k_nn and k_cv are both numeric, if not, stop and show error information
  if (!is.numeric(k_nn) || !is.numeric(k_cv)) {
    stop("k_nn and k_cv must be numeric")
  }
  # split data in k_cv parts, randomly
  fold <- sample(rep(1:k_cv, length = length(cl)))
  data <- data.frame("x" = train, "y" = cl, "split" = fold)
  class <- c()
  cv_err <- rep(NA, k_cv)
  fold_l <- length(cl) / k_cv
  # iterate through i = 1 to k_cv
  for (i in 1:k_cv) {
    # predict class value of the ith fold using all other folds as training data
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    y_hat <- knn(select(data_train, contains("x")), select(data_test, contains("x")), data_train$y, k_nn)
    class <- c(class, y_hat)
    # record the prediction and the misclassification rate
    cv_err[i] = sum(y_hat != data_test$y) / fold_l
  }
  y_hat <- knn(train, train, cl, k_nn)
  train_err = sum(as.numeric(y_hat != cl)) / length(cl)
  output <- list("class" = class, "cv_err" = mean(cv_err), "te" = train_err)
  return(output)
}
