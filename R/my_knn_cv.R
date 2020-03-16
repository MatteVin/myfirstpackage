#' K-Nearest Neighbors Cross-Validation.
#'
#' @param train the training data set
#' @param cl true class value of training data.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#'
#' @return A list with objects \code{class}, a vector of the predicted class
#' y_hat when all the data is used for both training and testing, and
#' \code{cv_err} a numeric with the average cross-validation misclassification
#' errorr.
#'
#' @keywords prediction
#'
#' @examples
#' my_knn_cv(train = my_iris[,-5], cl = my_iris[,5], k_nn = 1, k_cv = 5)
#' my_knn_cv(train = my_gapminder[,-4], cl = my_gapminder[,4], k_nn = 4, k_cv = 8)
#'
#' @import class stats dplyr
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  #creates folds to devide data between training and testing
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  #inizializes vecor containing the error for each iteration k_cv
  cv_err <- rep(NA, k_cv)
  for (i in 1:k_cv) {
    #splits the data between training and testing according to the folds
    data_test <- train[folds == i,]
    data_train <- train[folds != i,]
    #splits the true class value between training and testing according to
    #the folds
    cl_test <- cl[folds == i]
    cl_train <- cl[folds != i]
    #pedics the response of the test data suing k-Nearest Neighbors (k = k_nn)
    predict_cv <- knn(train = data_train,
                      test = data_test,
                      cl = cl_train,
                      k = k_nn
                      )
    #adds to the error list the rate of error of the model
    cv_err[i] <- sum(predict_cv != cl_test)/length(cl_test)
  }
  #stores the predictions of all the data using all the data for training.
  class <- knn(train = train, test = train, cl = cl, k = k_nn)
  cv_err <- mean(cv_err)
  #creates return list
  my_output <- list(class, cv_err)
  return(my_output)
}
