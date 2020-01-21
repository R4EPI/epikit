epikit
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epikit)](https://CRAN.R-project.org/package=epikit)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/zkamvar/epikit?branch=master&svg=true)](https://ci.appveyor.com/project/zkamvar/epikit)
[![Travis build
status](https://travis-ci.org/R4EPI/epikit.svg?branch=master)](https://travis-ci.org/R4EPI/epikit)
[![Codecov test
coverage](https://codecov.io/gh/R4EPI/epikit/branch/master/graph/badge.svg)](https://codecov.io/gh/R4EPI/epikit?branch=master)
<!-- badges: end -->

The goal of {epikit} is to provide miscellaneous functions for This is a
product of the R4EPIs project; learn more at
<https://r4epis.netlify.com>.

## Installation

<!--
You can install {epikit} from CRAN:

``` r
install.packages("epikit")
```

<details>
<!--
NOTE: everything inside the details tag will be collapsed and effectively
hidden from the user
<summary style='text-decoration: underline'>Click here for alternative installation options</summary>
If there is a bugfix or feature that is not yet on CRAN, you can install it via
the {drat} package:

-->

You can install {epikit} from the R4EPI repository:

``` r
# install.packages("drat")
drat::addRepo("R4EPI")
install.packages("epikit")
```

You can also install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/epikit") 
```

<!-- </details> -->

-----

``` r
library("epikit")
```

The {epikit} was primarily designed to house convenience functions for
field epidemiologists to use in tidying their reports. The functions in
{epikit} come in a few categories:

### Table modification

These functions all modify the appearance of a table displayed in a
report and work best with the `knitr::kable()` function.

  - `rename_redundant()` renames redundant columns with a single name.
    (e.g. `hopitalized_percent` and `confirmed_percent` can both be
    renamed to `%`)
  - `augment_redundant()` is similar to `rename_redundant()`, but it
    modifies the redundant column names (e.g. `hospitalized_n` and
    `confirmed_n` can become `hospitalized (n)` and `confirmed (n)`)
  - `merge_ci()` combines estimate, lower bound, and upper bound columns
    into a single column.

<!-- end list -->

``` r
library("knitr")
library("magrittr")
df <- data.frame(
  `a n` = 1:6,
  `a prop` = round((1:6) / 6, 2),
  `a deff` = round(pi, 2),
  `b n` = 6:1,
  `b prop` = round((6:1) / 6, 2),
  `b deff` = round(pi * 2, 2),
  check.names = FALSE
)
knitr::kable(df)
```

| a n | a prop | a deff | b n | b prop | b deff |
| --: | -----: | -----: | --: | -----: | -----: |
|   1 |   0.17 |   3.14 |   6 |   1.00 |   6.28 |
|   2 |   0.33 |   3.14 |   5 |   0.83 |   6.28 |
|   3 |   0.50 |   3.14 |   4 |   0.67 |   6.28 |
|   4 |   0.67 |   3.14 |   3 |   0.50 |   6.28 |
|   5 |   0.83 |   3.14 |   2 |   0.33 |   6.28 |
|   6 |   1.00 |   3.14 |   1 |   0.17 |   6.28 |

``` r
df %>%
  rename_redundant("%" = "prop", "Design Effect" = "deff") %>%
  augment_redundant(" (n)" = " n$") %>%
  knitr::kable()
```

| a (n) |    % | Design Effect | b (n) |    % | Design Effect |
| ----: | ---: | ------------: | ----: | ---: | ------------: |
|     1 | 0.17 |          3.14 |     6 | 1.00 |          6.28 |
|     2 | 0.33 |          3.14 |     5 | 0.83 |          6.28 |
|     3 | 0.50 |          3.14 |     4 | 0.67 |          6.28 |
|     4 | 0.67 |          3.14 |     3 | 0.50 |          6.28 |
|     5 | 0.83 |          3.14 |     2 | 0.33 |          6.28 |
|     6 | 1.00 |          3.14 |     1 | 0.17 |          6.28 |

### Quick proportions with conficence intervals

There are three functions that will provide quick statistics for
different rates based on binomial estimates of proportions from
`binom::binom.wilson()`

  - `attack_rate()`
  - `case_fatality_rate()`
  - `mortality_rate()`

<!-- end list -->

``` r
attack_rate(10, 50)
#>   cases population ar    lower    upper
#> 1    10         50 20 11.24375 33.03711
case_fatality_rate(2, 50)
#>   deaths population cfr    lower    upper
#> 1      2         50   4 1.103888 13.46009
mortality_rate(40, 50000)
#>   deaths population mortality per 10 000   lower    upper
#> 1     40      50000                    8 5.87591 10.89109
```

In addition, it’s possible to rapidly calculate Case fatality rate from
a linelist, stratified by different groups (e.g. gender):

``` r
library("outbreaks")
case_fatality_rate_df(ebola_sim$linelist, 
  outcome == "Death", 
  group = gender,
  add_total = TRUE,
  mergeCI = TRUE
)
#> # A tibble: 3 x 5
#>   gender deaths population   cfr ci            
#>   <fct>   <int>      <int> <dbl> <chr>         
#> 1 f        1301       2962  43.9 (42.14--45.72)
#> 2 m        1281       2926  43.8 (41.99--45.58)
#> 3 Total    2582       5888  43.9 (42.59--45.12)
```

### Inline functions

The inline functions make it easier to print estimates with confidence
intervals in reports with the correct number of digits.

  - `fmt_ci()` formats confidence intervals from three numbers. (e.g.
    `fmt_ci(50, 10, 80)` produces 50.00% (CI 10.00–80.00)
  - `fmt_pci()` formats confidence intervals from three fractions,
    multiplying by 100 beforehand.

The `_df` suffixes (`fmt_ci_df()`, `fmt_pci_df()`) will print the
confidence intervals for data stored in data frames. These are designed
to work with the outputs of the rates functions. For example,
`fmt_ci_df(attack_rate(10, 50))` will produce 20.00% (CI 11.24–33.04).

  - `fmt_count()` will count a condition in a data frame and present the
    number and percent of `TRUE` values.
