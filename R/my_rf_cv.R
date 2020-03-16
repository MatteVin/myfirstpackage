#' Random Forest Cross-Validation
#'
#' description.
#'
#' @param  k something something.
#' @return something must be returned.
#' @keywords shower
#' @import randomForest dplyr magrittr
#'
#' @export
#My Random Forest Cross-Validation function
my_rf_cv <- function(k) {
  fold <- sample(rep(1:k, length = length(my_gapminder$lifeExp)))
  # data <- data.frame()
  mse <- rep(NA, k)
  # loop thru the folds
  for (i in 1:k) {
    data_train <- iris[fold != i, ] # Xi
    data_test <-  iris[fold == i, ]  # Xi star
    # Train our models
    cl_train <- my_gapminder$lifeExp[fold != i] # Yi
    cl_test <- my_gapminder$lifeExp[fold == i]  # Yi star
    model <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 100)
    predictions <- predict(model, data_test[, -1])
    mse[i] <- mean((predictions - cl_test)^2)
  }
  output <- mean(mse)
}
