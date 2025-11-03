# Test setup data
create_test_data <- function() {
  data.frame(
    # Dates within period
    s1 = as.Date(c("2013-01-01", "2013-01-05", "2013-01-10", NA, NA, "2012-12-15")),
    s2 = as.Date(c("2013-01-03", NA, "2013-01-08", "2013-01-02", NA, "2014-01-15")),
    s3 = as.Date(c(NA, "2013-01-09", "2013-01-01", "2013-01-05", NA, "2013-01-05")),
    # Period boundaries
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-10"),
    stringsAsFactors = FALSE
  )
}

test_that("find_date_cause handles dates within period correctly", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe)
  )

  # Row 1: s1 is first valid date
  expect_equal(result$start_date[1], as.Date("2013-01-01"))
  expect_equal(result$start_date_reason[1], "s1")

  # Row 2: s1 is first, s2 is NA, s3 is valid
  expect_equal(result$start_date[2], as.Date("2013-01-05"))
  expect_equal(result$start_date_reason[2], "s1")

  # Row 3: s1 is first valid
  expect_equal(result$start_date[3], as.Date("2013-01-10"))
  expect_equal(result$start_date_reason[3], "s1")

  # Row 4: s1 is NA, s2 is valid
  expect_equal(result$start_date[4], as.Date("2013-01-02"))
  expect_equal(result$start_date_reason[4], "s2")
})

test_that("find_date_cause respects column order for ties", {
  d <- data.frame(
    s1 = as.Date(c("2013-01-05", "2013-01-05")),
    s2 = as.Date(c("2013-01-05", NA)),
    s3 = as.Date(c("2013-01-05", "2013-01-05")),
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-10")
  )

  result <- find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe)

  # When dates are tied, first column wins
  expect_equal(result$start_date_reason[1], "s1")

  # Test different order
  result2 <- find_date_cause(d, s3, s2, s1, period_start = ps, period_end = pe)
  expect_equal(result2$start_date_reason[1], "s3")
})

test_that("find_date_cause with na_fill = NULL leaves NAs", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, s3,
                    period_start = ps, period_end = pe,
                    na_fill = NULL)
  )

  # Row 5: all NAs, should stay NA
  expect_true(is.na(result$start_date[5]))
  expect_equal(result$start_date_reason[5], "")

  # Row 6: s1 before period (NA), s2 after period (NA), s3 within period (valid)
  expect_equal(result$start_date[6], as.Date("2013-01-05"))
  expect_equal(result$start_date_reason[6], "s3")
})

test_that("find_date_cause with na_fill = 'start' fills NAs with period_start", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, s3,
                    period_start = ps, period_end = pe,
                    na_fill = "start")
  )

  # Row 5: all NAs, should get period_start
  expect_equal(result$start_date[5], as.Date("2013-01-01"))
  expect_equal(result$start_date_reason[5], "period_start")
})

test_that("find_date_cause with na_fill = 'end' fills NAs with period_end", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, s3,
                    period_start = ps, period_end = pe,
                    na_fill = "end")
  )

  # Row 5: all NAs, should get period_end
  expect_equal(result$start_date[5], as.Date("2013-01-10"))
  expect_equal(result$start_date_reason[5], "period_end")
})

test_that("find_date_cause warns when filling with dates outside period", {
  d <- create_test_data()

  # Row 6 has s1 before period and s2 after period
  # The new warning explicitly reports ignored before/after period dates
  expect_warning(
    find_date_cause(d, s1, s2, s3,
                    period_start = ps, period_end = pe,
                    na_fill = "start"),
    "Row 6: ignored date\\(s\\) before period_start and after period_end while selecting in-period date 2013-01-05"
  )

  expect_warning(
    find_date_cause(d, s1, s2, s3,
                    period_start = ps, period_end = pe,
                    na_fill = "end"),
    "Row 6: ignored date\\(s\\) before period_start and after period_end while selecting in-period date 2013-01-05"
  )
})

test_that("find_date_cause warns when ignoring out-of-period dates", {
  d <- create_test_data()
  # Make row 6 have one valid in-period and one before-period
  d$s1[6] <- as.Date("2012-12-15")
  d$s2[6] <- as.Date(NA)
  d$s3[6] <- as.Date("2013-01-05")

  expect_warning(
    find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe),
    "ignored date\\(s\\) before period_start"
  )
})


test_that("find_date_cause issues distinct warnings for ignored vs filled cases", {


  # --- Case 1: In-period date selected, but some out-of-period dates ignored ---
  d1 <- data.frame(
    s1 = as.Date("2012-12-20"),
    s2 = as.Date("2013-01-05"),
    s3 = as.Date("2013-02-10"),
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-31")
  )

  expect_warning(
    find_date_cause(
      d1,
      s1, s2, s3,
      period_start = ps, period_end = pe
    ),
    "ignored date\\(s\\) before period_start and after period_end"
  )

  # --- Case 2: No valid in-period date → filled with period_start ---
  d2 <- data.frame(
    s1 = as.Date("2013-02-02"),
    s2 = as.Date("2013-02-10"),
    s3 = as.Date(NA),
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-31")
  )

  expect_warning(
    find_date_cause(
      d2,
      s1, s2, s3,
      period_start = ps, period_end = pe,
      na_fill = "start"
    ),
    "^\\*?\\s*1 row\\(s\\) had valid dates after period_end but were filled with period_start: rows 1"
  )

  # --- Case 3: No valid in-period date → filled with period_end ---
  d3 <- data.frame(
    s1 = as.Date("2012-12-10"),
    s2 = as.Date("2012-12-15"),
    s3 = as.Date(NA),
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-31")
  )

  expect_warning(
    find_date_cause(
      d3,
      s1, s2, s3,
      period_start = ps, period_end = pe,
      na_fill = "end"
    ),
    "^\\*?\\s*1 row\\(s\\) had valid dates before period_start but were filled with period_end: rows 1"
  )
})


test_that("find_start_date wrapper works correctly", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_start_date(d, s1, s2, s3,
                    period_start = ps, period_end = pe)
  )

  expect_true("start_date" %in% names(result))
  expect_true("start_date_reason" %in% names(result))

  # Should fill with start by default
  expect_equal(result$start_date[5], as.Date("2013-01-01"))
  expect_equal(result$start_date_reason[5], "period_start")
})

test_that("find_end_date wrapper works correctly", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_end_date(d, s1, s2, s3, period_start = ps, period_end = pe)
  )

  expect_true("end_date" %in% names(result))
  expect_true("end_date_reason" %in% names(result))

  # Should fill with end by default
  expect_equal(result$end_date[5], as.Date("2013-01-10"))
  expect_equal(result$end_date_reason[5], "period_end")
})

test_that("find_date_cause errors on non-date columns with helpful message", {
  d <- create_test_data()
  d$not_a_date <- "2013-01-01"  # Character, not Date

  expect_error(
    find_date_cause(d, s1, not_a_date, s3, period_start = ps, period_end = pe),
    "The following columns are not Date objects: not_a_date"
  )

  # Multiple non-date columns
  d$also_not_date <- 123
  expect_error(
    find_date_cause(d, not_a_date, also_not_date, period_start = ps, period_end = pe),
    "not_a_date, also_not_date"
  )
})

test_that("find_date_cause errors when period columns are not dates", {
  d <- create_test_data()
  d$bad_start <- "2013-01-01"

  expect_error(
    find_date_cause(d, s1, s2, period_start = bad_start, period_end = pe),
    "The following columns are not Date objects: bad_start"
  )
})

test_that("constrain_dates works with boundary = 'both'", {
  dates <- as.Date(c("2012-12-31", "2013-01-05", "2013-01-15"))
  ps <- as.Date("2013-01-01")
  pe <- as.Date("2013-01-10")

  result <- constrain_dates(dates, ps, pe, boundary = "both")

  expect_true(is.na(result[1]))  # Before period
  expect_equal(result[2], as.Date("2013-01-05"))  # Within period
  expect_true(is.na(result[3]))  # After period
})

test_that("constrain_dates works with boundary = 'start'", {
  dates <- as.Date(c("2012-12-31", "2013-01-05", "2013-01-15"))
  ps <- as.Date("2013-01-01")
  pe <- as.Date("2013-01-10")

  result <- constrain_dates(dates, ps, pe, boundary = "start")

  expect_equal(result[1], as.Date("2013-01-01"))  # Replaced with period_start
  expect_equal(result[2], as.Date("2013-01-05"))  # Within period
  expect_true(is.na(result[3]))  # After period becomes NA
})

test_that("constrain_dates works with boundary = 'end'", {
  dates <- as.Date(c("2012-12-31", "2013-01-05", "2013-01-15"))
  ps <- as.Date("2013-01-01")
  pe <- as.Date("2013-01-10")

  result <- constrain_dates(dates, ps, pe, boundary = "end")

  expect_true(is.na(result[1]))  # Before period becomes NA
  expect_equal(result[2], as.Date("2013-01-05"))  # Within period
  expect_equal(result[3], as.Date("2013-01-10"))  # Replaced with period_end
})

test_that("constrain_dates handles NA values correctly", {
  dates <- as.Date(c(NA, "2013-01-05", NA))
  ps <- as.Date("2013-01-01")
  pe <- as.Date("2013-01-10")

  result <- constrain_dates(dates, ps, pe, boundary = "both")

  expect_true(is.na(result[1]))
  expect_equal(result[2], as.Date("2013-01-05"))
  expect_true(is.na(result[3]))
})

test_that("assert_positive_timespan returns NULL when all timespans positive", {
  d <- data.frame(
    start = as.Date(c("2013-01-01", "2013-01-05")),
    end = as.Date(c("2013-01-10", "2013-01-15"))
  )

  result <- assert_positive_timespan(d, start, end)
  expect_null(result)
})

test_that("assert_positive_timespan warns and returns problematic rows", {
  d <- data.frame(
    start = as.Date(c("2013-01-10", "2013-01-05", "2013-01-01")),
    end = as.Date(c("2013-01-05", "2013-01-15", "2013-01-10"))
  )

  expect_warning(
    result <- assert_positive_timespan(d, start, end),
    "1 rows had negative timespans"
  )

  # Should return the problematic row(s)
  result_data <- suppressWarnings(assert_positive_timespan(d, start, end))
  expect_equal(nrow(result_data), 1)
  expect_equal(result_data$start[1], as.Date("2013-01-10"))
})

test_that("find_date_cause handles custom column names", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, s3,
                    period_start = ps, period_end = pe,
                    datecol = "my_date",
                    datereason = "my_reason")
  )

  expect_true("my_date" %in% names(result))
  expect_true("my_reason" %in% names(result))
  expect_false("start_date" %in% names(result))
})

test_that("find_date_cause handles vectorized period boundaries", {
  d <- data.frame(
    s1 = as.Date(c("2013-01-05", "2014-01-05")),
    s2 = as.Date(c("2013-01-08", "2014-01-08")),
    ps = as.Date(c("2013-01-01", "2014-01-01")),
    pe = as.Date(c("2013-01-10", "2014-01-10"))
  )

  result <- find_date_cause(d, s1, s2, period_start = ps, period_end = pe)

  expect_equal(result$start_date[1], as.Date("2013-01-05"))
  expect_equal(result$start_date[2], as.Date("2014-01-05"))
})

test_that("find_date_cause preserves original data columns", {
  d <- create_test_data()
  d$extra_col <- 1:6

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, period_start = ps, period_end = pe)
  )

  expect_true("extra_col" %in% names(result))
  expect_equal(result$extra_col, d$extra_col)
})

test_that("find_date_cause adds columns in correct position", {
  d <- create_test_data()

  result <- suppressWarnings(
    find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe)
  )

  # Should be added before first date column (s1)
  s1_pos <- which(names(result) == "s1")
  date_pos <- which(names(result) == "start_date")

  expect_true(date_pos < s1_pos)
})

test_that("warnings are not triggered when fills are appropriate", {
  # All NAs - no warning should be triggered
  d <- data.frame(
    s1 = as.Date(c(NA, NA)),
    s2 = as.Date(c(NA, NA)),
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-10")
  )

  expect_no_warning(
    find_date_cause(d, s1, s2, period_start = ps, period_end = pe, na_fill = "start")
  )
})

test_that("edge case: all dates equal to boundaries", {
  d <- data.frame(
    s1 = as.Date(c("2013-01-01", "2013-01-10")),
    ps = as.Date("2013-01-01"),
    pe = as.Date("2013-01-10")
  )

  result <- find_date_cause(d, s1, period_start = ps, period_end = pe)

  expect_equal(result$start_date[1], as.Date("2013-01-01"))
  expect_equal(result$start_date[2], as.Date("2013-01-10"))
  expect_equal(result$start_date_reason[1], "s1")
  expect_equal(result$start_date_reason[2], "s1")
})
