
test_that("five or fewer numbers will be preserved as factors", {
  n <- sample(100, 5) 
  d <- n + runif(5)
  expect_equal(fac_from_num(n), factor(n))
  expect_equal(fac_from_num(d), factor(d))
})

test_that("six or more numbers will be preserved as factors", {
  n <- sample(100)
  n[n == 42] <- NA
  nf <- cut(n, breaks = pretty(range(n, na.rm = TRUE)), include.lowest = TRUE)
  expect_equal(fac_from_num(n), nf)
})

test_that("only numeric vectors can pass", {
  expect_error(fac_from_num(letters), 
               "fac_from_num() only uses integer or numeric vectors.",
               fixed = TRUE
  )
})
