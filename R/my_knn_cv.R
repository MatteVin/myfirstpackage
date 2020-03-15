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
#' @examples
#' my_knn_cv(train = my_iris[,-5], cl = my_iris[,5], k_nn = 1, k_cv = 5)
#' my_knn_cv(train = my_iris[,-5], cl = my_iris[,5], k_nn = 4, k_cv = 5)
#' @import stats class dplyr magrittr
#' @export
#My k-Nearest Neighbors Cross-Validation function
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Split data in k_cv parts, randomly
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  # Combines input data
  data <- data.frame(train, cl, folds)
  #initializes cv error as 0
  cv_err <- 0
  #loops trough the different folds testing one against the rest
  for(i in 1:k_cv){
    #store training data for the iteration
    data_train <- data %>% filter(folds != i)
    #records the response for the training data
    cl_train <- data_train %>% select(cl)
    #eliminates the folds and responses from the data
    data_train <- data_train %>% select(-cl, -folds)
    #store testing data for the iteration
    data_test <- data %>% filter(folds == i)
    #records the response for the testing data
    cl_test <- data_test %>% select(cl)
    #eliminates the folds and responses from the data
    data_test <- data_test %>% select(-cl, -folds)
    #pedics the response of the test data suing k-Nearest Neighbors (k = k_nn)
    predict_cv <- knn(train = data_train,
                      cl = cl_train,
                      test = data_test,
                      k = k_nn
    )
    #calculates and compunds the average cv errro
    cv_err <- ((sum(ifelse(cl_test == predict_cv, 0, 1)) / length(cl_test)) +
                 cv_err * (i - 1)) / i
  }
  #stores the predictions of all the data using all the data for training.
  class <- knn(train = train, cl = cl, test = train, k = k_nn)
  #return results
  result <- list("class" = class, "cv_err" = cv_err)
  return(result)
}
