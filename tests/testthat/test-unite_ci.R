
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
df  <- data.frame(v = names(coef(fit)), e = coef(fit), confint(fit), row.names = NULL)
names(df) <- c("variable", "estimate", "lower", "upper")

test_that("Unite CI will unite the colums by name", {
  united <- unite_ci(df, "slope (CI)", estimate, lower, upper, m100 = FALSE, percent = FALSE)
  expect_length(united, 2)
  expect_named(united, c("variable", "slope (CI)"))
  expected <- c(
    "0.74 (-0.77--2.26)", 
    "0.00 (-0.00--0.01)", 
    "0.01 (-0.00--0.01)", 
    "1.00 (0.38--1.62)", 
    "0.16 (-0.61--0.93)"
  )
  expect_identical(united[[2]], expected)
  expect_identical(united[[1]], df[[1]])

})
