test_that("find_breaks works with default values", {
  expect_equal(find_breaks(100), c(0, 25, 50, 75) + 1)
  expect_equal(find_breaks(100, ceiling = TRUE), c(0, 25, 50, 75, 99) + 1)
})

test_that("find_breaks won't break down for missing", {
  expect_error(find_breaks(NA), "n must be a single finite number")
  expect_error(find_breaks(100, breaks = NA), "breaks must be a single, positive finite number")
  expect_error(find_breaks(100, snap = NA), "snap must be a single, positive finite number")
})

test_that("snap must be smaller than n", {
  expect_error(find_breaks(100, snap = 1000), "snap (1000) must be smaller than n (100)", fixed = TRUE)
})

test_that("breaks will work with snapping", {
  expect_equal(find_breaks(100, snap = 20), c(0, 40, 80) + 1)
  expect_equal(find_breaks(100, snap = 20, ceiling = TRUE), c(0, 40, 80, 99) + 1)

  expect_equal(find_breaks(123, snap = 20), c(0, 40, 80, 120) + 1)
  expect_equal(find_breaks(123, snap = 20, ceiling = TRUE), c(0, 40, 80, 120, 122) + 1)

  expect_equal(find_breaks(123, snap = 25), c(0, 50, 100) + 1)
  expect_equal(find_breaks(123, snap = 25, ceiling = TRUE), c(0, 50, 100, 122) + 1)
})
