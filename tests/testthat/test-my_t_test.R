test_that("my_t_test outputs a list", {
  expect_is(my_t_test(1:20, "less", 15),"list")
})
test_that("my_t_test works matematically", {
  test <- t.test(1:20, alternative = "two.sided", mu = 18)
  comparason <- unname(list(unname(test[[1]]),
                              unname(test[[2]]),
                              test[[8]],
                              test[[3]]
                              )
                       )
  expect_equal(unname(my_t_test(1:20, "two.sided", 18)),comparason)
})
test_that("my_t_test outputs error when icorrect parameter is used", {
  expect_error(my_t_test("one.sided", "two.sided", "three.sided"))
})
