test_that("my_lm works", {
  my_model <- my_lm(lifeExp~ gdpPercap + continent, my_gapminder)
  r_model <- summary(lm(lifeExp~ gdpPercap + continent, my_gapminder))
  expect_equal(matrix(my_model[, -4]), matrix(r_model$coefficients[, -4]))
})
test_that("incorrect input throws error", {
  expect_error(my_lm(formula = continent ~ lifeExp ,
                     data = my_gapminder))
  expect_error(my_lm(formula =  lifeExpectancy ~ gdpPerCapita + Continent ,
                     data = my_gapminder))
})
