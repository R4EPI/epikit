
cfr <- case_fatality_rate(10, 50)
cfr_expected <- "20.00% (CI 11.24-33.04)"
cfr_merged <- gsub("^[0-9.% (CI]{11}", "(", cfr_expected)
pro <- proportion(5, 50)
pro_expected <- "10.00% (CI 4.35-21.36)"
pro_merged <- gsub("^[0-9.% (CI]{11}", "(", pro_expected)

test_that("fmt_ci.default only accepts numbers", {
  expect_error(fmt_pci(0 , 0  , 0  , "A"))
  expect_error(fmt_ci(0  , 0  , "A", 0))
  expect_error(fmt_ci(0  , "A", 0  , 0))
  expect_error(fmt_ci("A", 0  , 0  , 0))
})

test_that("fmt_ci will treat missing data without messing up", {
  expect_identical(fmt_ci(1        , NA_real_, NA_real_), "1.00% (CI NA-NA)")
  expect_identical(fmt_ci(1        , 0       , NA_real_), "1.00% (CI 0.00-NA)")
  expect_identical(fmt_ci(1        , 0       , Inf), "1.00% (CI 0.00-Inf)")
  expect_identical(fmt_ci(NA_real_ , NA_real_, NA_real_), "NA (CI NA-NA)")
  expect_identical(fmt_pci(1       , NA_real_, NA_real_), "100.00% (CI NA-NA)")
  expect_identical(fmt_pci(NA_real_, NA_real_, NA_real_), "NA (CI NA-NA)")
})

test_that("fmt_ci gives expected results", {
  expect_identical(fmt_ci(pi, pi, pi, 2), "3.14% (CI 3.14-3.14)")
  expect_identical(fmt_ci(pi, pi, pi, 3), "3.142% (CI 3.142-3.142)")
})

test_that("fmt_p?ci_df can take data frames", {
  cfr100       <- data.frame(lapply(cfr, `/`, 100))
  expect_identical(fmt_ci_df(cfr),        cfr_expected)
  expect_identical(fmt_ci_df(cfr[-1], 2), cfr_expected)
  expect_identical(fmt_pci_df(cfr100),    cfr_expected)
})

test_that("merge_p?ci will adjust data frames", {

  cfr100       <- data.frame(lapply(cfr, `/`, 100))

  # merge_ci_df() -----------------------------------------------
  merged_res <- merge_ci_df(cfr)
  expect_is(merged_res, "data.frame")
  expect_named(merged_res, c(names(cfr)[1:3], "ci"))
  expect_identical(merged_res$ci, cfr_merged)

  # merge_pci_df() ----------------------------------------------
  merged_res <- merge_pci_df(cfr100)
  expect_is(merged_res, "data.frame")
  expect_named(merged_res, c(names(cfr)[1:3], "ci"))
  expect_identical(merged_res$ci, cfr_merged)
})

test_that("fmt_ci_df will produce multiple results when given multiple rows", {
  cfro <- rbind(cfr, setNames(pro, names(cfr)))
  expect_length(fmt_ci_df(cfro), 2)
  expect_identical(fmt_ci_df(cfro), c(cfr_expected, pro_expected))
  expect_identical(merge_ci_df(cfro)$ci, c(cfr_merged, pro_merged))
})

test_that("fmt_count() works as expected", {

  ayeris <- iris
  ayeris$Species[1] <- NA
  expect_identical(fmt_count(iris, Species == "virginica"), "50 (33.3%)")
  expect_identical(fmt_count(ayeris, Species == "virginica"), "50 (33.3%)")
  expect_identical(
    fmt_count(ayeris, Species == "virginica", Petal.Length > 6),
    "9 (6.0%)"
  )
  expect_identical(
    fmt_count(ayeris, Species == "virginica" | Petal.Length > 6),
    "50 (33.3%)"
  )

})
