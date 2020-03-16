#' Student's t-test
#'
#' @export
#t-test function
my_t_test <- function(x, alternative, mu) {
  #check for the parameter alternative to be one of the three allowed, if it
  #isn't give a warning.
  if(!(alternative %in% c("two.sided", "less", "greater"))){
    warning("alternative not recognized")
  }
  #saves the length of the vector as n
  n <- length(x)
  #calculates the t-statistic
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(n))
  #calculates the degrees of freedom
  df <- n - 1
  #calculate p_val lower tail.
  p_val <- pt(test_stat, df)
  #change p_val to upper tail.
  p_val <- if(grepl(alternative, "greater")){
    (1 - p_val)
    #change p_val to both tails.
  } else if(grepl(alternative, "two.sided")){
    min(p_val, 1 - p_val) * 2
  } else { p_val }
  #combines the results in one list
  result <- list("test_stat" = test_stat,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_val
  )
  #returns the results
  return(result)
}
