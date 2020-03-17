test_that("my_rf_cv outputs is numeric", {
  expect_is(my_rf_cv(k = 3), "numeric")
})
test_that("non-numeric input throws error", {
  expect_error(my_rf_cv("mistake"))
})
test_that("incorrect input throws error", {
  expect_error(my_rf_cv(1))
})
