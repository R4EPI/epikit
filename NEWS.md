# epikit 0.1.6 

* internal documentation fix. No user-visible changes.

# epikit 0.1.5 

* removed tidyr::unite() in favour of paste-ing two columns together
* added `separator` argument to `_ci()` functions which now defaults to `-`
  (@aspina7, #31)

# epikit 0.1.4 

* unecessary bump because {pkgdown}

# epikit 0.1.3

(non-cran release)

* migrated functions and tests from {sitrep} including: `add_weights_cluster()`, 
  `add_weights_strata()`, `find_date_cause()`, `find_end_date()`, 
  `find_start_date()`, `gen_polygon()`, `gen_population()` and `zcurve()` (@aspina7, #22)
* update documentation and pkgdown site (@aspina7, #23)

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
