context("age_categories()")

ages <- c(28L, 64L, 16L, 88L, 32L, 81L, 77L, 59L, 25L, 7L)
test_that("age_categories will collapse in increments of 10 years by default", {
  expect_equal(as.character(age_categories(ages, upper = 100)),
               c('20-29', '60-69', '10-19', '80-89', '30-39', '80-89', '70-79', '50-59', '20-29', '0-9'))
})

