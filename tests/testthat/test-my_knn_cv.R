test_that("my_knn_cv outputs a list", {
  expect_is(my_knn_cv(train = my_gapminder[,5:6],
                      cl = my_gapminder$continent,
                      k_nn = 3,
                      k_cv = 10
                      ),
            "list"
  )
})
test_that("non-list input throws error", {
  expect_error(my_knn_cv("defenetly not a list"))
})
test_that("non-numeric-positive input for k_nn and k throws error", {
  expect_error(my_knn_cv(train = my_iris[, -5], cl = my_iris$Species, -1, -4))
  expect_error(my_knn_cv(train = my_iris[, -5], cl = my_iris$Species, "a", 5))
  expect_error(my_knn_cv(train = my_iris[, -5], cl = my_iris$Species, 3, "b"))
})
