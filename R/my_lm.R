#' Fitting Linear Models
#'
#' This function is used to fit linear models. It can be used to carry out
#' regressions.
#'
#' @param formula a \code{formula} class object: a symbolic description of the
#' model to be fitted.
#' @param data input data frame.
#'
#' @return A table containing all numerics, with one row for each coefficient
#' including the intercept and colums for the \code{Estimate}, the
#' \code{Std. Error}, the \code{t value}, and \code{Pr(>|t|)} (the p-value for
#' the t-test).
#'
#' @examples
#' my_lm(Sepal.Length ~ Sepal.Width, data = my_iris))
#' my_lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = my_iris))
#'
#' @keywords
#'
#' @import stats
#'
#' @export
#Creates my_lm a function that takes in as parameters a formula and a dataset
#returning a table containing the appropiate coefficients for the linear model.
my_lm <- function(formula, data) {
  #Extracts model matrix X.
  X_lm <- model.matrix(formula, data)
  #Extract a model respopnse Y.
  Y_lm <- model.response(model.frame(data))
  #Estimates linear regression coefficients.
  beta_lm <- solve(t(X_lm) %*% X_lm) %*%  t(X_lm) %*% Y_lm
  #Estimates the degrees of freedom.
  df_lm <- nrow(data) - nrow(beta_lm)
  #Estimates the variance.
  var_lm <- sum(((Y_lm - X_lm %*% beta_lm )^2)) / df_lm
  #Estimates the standard error.
  suppressWarnings(std_error_lm <- diag(sqrt(var_lm * solve(t(X_lm) %*% X_lm))))
  #Estimates the t value.
  t_value_lm <- beta_lm / std_error_lm
  #Estimates the p value of the t test.
  p_of_t <- pt(abs(t_value_lm), df_lm, lower.tail = FALSE)*2
  #Combines coefficients, standard errors, t values and p values of the t test,
  #all in one matrix
  final_matrix <- cbind(beta_lm, std_error_lm, t_value_lm, p_of_t)
  #appropiatly names the culums of the matrix
  colnames(final_matrix) <-  c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  #returns the final matrix as a table
  return(as.table(final_matrix))
}
