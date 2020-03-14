#' Random Forest Cross-Validation
#'
#' description
#'
#' @param  k
#'
#' @return
#'
#' @examples
#'
#' @keywords
#'
#' @import randomForest dplyr
#'
#' @export
#My Random Forest Cross-Validation function
my_rf_cv <- function(k){
  # Split data in k parts, randomly
  folds <- sample(rep(1:k, length = nrow(my_gapminder)))
  # Use data from before, leaving out species as we won't need it
  data <- data.frame(my_gapminder[,6], my_gapminder[,5], folds)
  #List for string cv errors on each iteration
  cv_err_list <- rep(NA, k)
  #loops trough the groups
  for(i in 1:k) {
    #set the training set
    data_train <- data %>% filter(folds != i)  %>% select(-folds)
    #set the testing set
    data_test <- data %>% filter(folds == i)  %>% select(-folds)
    #crates a random forest model on the training data
    model_k_i <- randomForest(lifeExp ~ gdpPercap,
                              data = data_train,
                              ntree = 100
                              )
    #calculates predictions on the test using the random forest model
    predictions_k_i <- predict(model_k_i, data_test["lifeExp"])
    #calculates the mean square error petween the predicted and true sepal.length
    cv_err_list[i] <- sum((predictions_k_i - data_test["gdpPercap"])^2) /
      (length(data_test[]))
  }
  #calculates and returns the mean cross validation MSE
  cv_err <- mean(cv_err_list)
  return(cv_err)
}
