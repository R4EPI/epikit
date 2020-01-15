context("age_categories()")

ages <- c(28L, 64L, 16L, 88L, 32L, 81L, 77L, 59L, 25L, 7L)

make_interval <- function(by = 10, from = 0, to = 100) {
  x <- seq(from = from, to = to, by = by)
  res <- apply(matrix(c(x, x + by - 1), ncol = 2), 1, paste, collapse = "-")
  res[length(res)] <- sprintf("%s+", x[length(x)])
  res
}

test_that("age_categories throws errors if neither breaks nor upper is specified", {

  expect_error(age_categories(ages), "one of `breakers` or `upper` must be specified")
  expect_error(age_categories(ages, breakers = 10), "please specify at least three breakers")
  expect_error(age_categories(ages, breakers = 1:2), "please specify at least three breakers")
  expect_error(age_categories(ages, breakers = c(NA, 1:2), upper = 1), "please specify at least three breakers")
  expect_error(age_categories(ages, breakers = c(5, 10, 20), upper = 1:2), "please only specify a single upper value")
  expect_error(age_categories(ages, upper = NA), "please only specify a single upper value")


})

test_that("age_categories will collapse in increments of 10 years by default", {
  expected <- factor(
    x = c(
      '20-29',
      '60-69',
      '10-19',
      '80-89',
      '30-39',
      '80-89',
      '70-79',
      '50-59',
      '20-29',
      '0-9'
    ),
    levels = make_interval(10, 0, 100)
  )
  expect_equal(age_categories(ages, upper = 100), expected)
  expected <- factor(
    x = c(
      '20-29',
      '60-69',
      '10-19',
      '80+',
      '30-39',
      '80+',
      '70-79',
      '50-59',
      '20-29',
      '0-9'
    ),
    levels = make_interval(10, 0, 80)
  )
  expect_equal(age_categories(ages, upper = 80), expected)
})

test_that("age_categories will take specific breakpoints", {
  expected <- factor(
    x = c(
      '20-29',
      '60-69',
      '10-19',
      '80-89',
      '30-39',
      '80-89',
      '70-79',
      '50-59',
      '20-29',
      '0-9'
    ),
    levels = make_interval()
  )
  expect_equal(age_categories(ages, breakers = c(0:10) * 10), expected)
  ages <- c(28L, 64L, 16L, 88L, 32L, 81L, 77L, 59L, 25L, 7L)
  expected <- factor(
    x = c(
      '25-29',
      '60-64',
      '15-19',
      '85-89',
      '30-34',
      '80-84',
      '75-79',
      '55-59',
      '25-29',
      '5-9'
    ),
    levels = make_interval(5, 0, 100)
  )
})

test_that("age_categories() will take customization", {
  # ceiling tests
  expected <- factor(
    x = c(
      '20-29',
      '60-69',
      '10-19',
      '80-89',
      '30-39',
      '80-89',
      '70-79',
      '50-59',
      '20-29',
      '0-9'
    ),
    levels = gsub("99", "100", make_interval(10, 0, 100)[1:10])
  )
  expect_equal(age_categories(ages, upper = 100, ceiling = TRUE), expected)

  # separator tests
  expected <- factor(
    x = gsub("-", ":)", c(
      '20-29',
      '60-69',
      '10-19',
      '80-89',
      '30-39',
      '80-89',
      '70-79',
      '50-59',
      '20-29',
      '0-9'
    )),
    levels = gsub("-", ":)", make_interval(10, 0, 100))
  )
  expect_equal(age_categories(ages, upper = 100, separator = ":)"), expected)

  # above.char tests
  expected <- factor(
    x = c(
      '20-29',
      '60-69',
      '10-19',
      '80-89',
      '30-39',
      '80-89',
      '70-79',
      '50-59',
      '20-29',
      '0-9'
    ),
    levels = gsub("[+]", "and on", make_interval(10, 0, 100))
  )
  expect_equal(age_categories(ages, upper = 100, above.char = "and on"), expected)
})


context("group_age_categories")

gen_years <- function() {
  age_categories(10 * c(0:9, rep(NA, 20)), upper = 100, by = 10)
}

gen_months <- function() {
  age_categories(6 * c(1, rep(NA, 9), 1:4, 1:4, 1:0, rep(NA, 10)), by = 6, upper = 24)
}

gen_days <- function() {
  age_categories(c(rep(NA, 19), 5:15), by = 2, upper = 15)
}

df <- data.frame(years = gen_years(), months = gen_months(), days = gen_days(), extra = 1:30)

test_that("extra columns are left alone", {

  expect_equal(group_age_categories(df, years = years, months = months)$extra, df$extra)

})

test_that("errors are thrown for gac", {
  expect_error(group_age_categories(df$years), "dat must be a data frame")
  expect_error(group_age_categories(df), "please specify one or more columns")
})

test_that("years alone give years", {
  years <- group_age_categories(df, years = years, one_column = FALSE)
  expect_equal(years$age_category, df$years)
})

test_that("months alone give months", {
  months <- group_age_categories(df, months = months, one_column = FALSE)
  expect_equal(months$age_category, df$months)
})

test_that("days alone give days", {
  days <- group_age_categories(df, days = days, one_column = FALSE)
  expect_equal(days$age_category, df$days)
})

test_that("levels are combined", {
  res <- group_age_categories(df, years = years, months = months, days = days)
  expect_match(levels(res$age_category), "\\d{1,3}[+-]\\d{0,3}? (days|months|years)")
  expect_failure(expect_match(levels(res$age_category), "0-9 years"))
  expect_equal(as.character(res$age_category[1]), "6-11 months")
  expect_equal(as.character(res$age_category[20]), "4-5 days")
})

