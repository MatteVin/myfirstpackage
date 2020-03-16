#' K-Nearest Neighbors Cross-Validation.
#'
#' @import class stats dplyr
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  n <- nrow(train)
  inds <- sample(rep(1:k_cv, length = n))
  cv_err <- rep(NA, k_cv)
  for (i in 1:k_cv) {
    #splits the data and true species into training and testing sets
    data_train <- train[inds != i,]
    data_test <- train[inds == i,]
    cl_train <- cl[inds != i]
    cl_test <- cl[inds == i]
    class_output <- knn(train = data_train,
                        test = data_test,
                        cl = cl_train, k = k_nn)
    #records errors of the predictions
    cv_err[i] <- sum(class_output != cl_test)/length(cl_test)
  }
  output <- knn(train = train, test = train, cl = cl, k = k_nn)
  my_output <- list(class_output, mean(cv_err))
  return(my_output)
}
