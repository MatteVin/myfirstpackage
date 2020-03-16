#' k-Nearest Neighbors Cross-Validation.
#' @import class stats dplyr
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Split data in k_cv parts, randomly
  folds <- sample(rep(1:k_cv, length = nrow(train)))
  # Combines input data
  data <- data.frame(cl, folds, train)
  #initializes cv error as 0
  cv_err <- 0
  #loops trough the different folds testing one against the rest
  for(i in 1:k_cv){
    my_gapminder <- my_gapminder
    #store training data for the iteration
    data_train <- data %>% filter(folds != i)
    #records the response for the training data
    cl_train <- data_train["cl"]
    #store testing data for the iteration
    data_test <- data %>% filter(folds == i)
    #records the response for the testing data
    cl_test <- data_test["cl"]
    #eliminates the folds and responses from the data
    data_train <- data_train[,-1]
    data_test <- data_test[,-1]
    data_train <- data_train[,-1]
    data_test <- data_test[,-1]
    data_train<- data.frame(data_train)
    data_test <- data.frame(data_test)
    #pedics the response of the test data suing k-Nearest Neighbors (k = k_nn)
    predict_cv <- knn(train =  data_train[complete.cases(data_train), ],
                      test = data_test[complete.cases(data_test), ],
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
