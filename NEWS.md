# epikit 0.1.2

* require version of {dplyr} to be >= 1.0.2
* require version of {tibble} to be >= 3.0.0
* update documentation
* fix issues stemming from {tibble} > 3.0.0

# epikit 0.1.1

* Internal update for compatibility for tibble 1.0.0 (@krlmlr, #19)
* Update tests for compatibility with dplyr 1.0.0 (@zkamvar, #20)

# epikit 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Update `case_fatality_rate_df()` to ignore missing values when tabulating the
  denominator (See https://github.com/R4EPI/epikit/issues/7 for details, Thanks to @thibautjombart for pointing this out).
