epikit
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/epikit)](https://CRAN.R-project.org/package=epikit)
[![Codecov test
coverage](https://app.codecov.io/gh/R4EPI/epikit/branch/master/graph/badge.svg)](https://app.codecov.io/gh/R4EPI/epikit?branch=master)
[![R-CMD-check](https://github.com/R4EPI/epikit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R4EPI/epikit/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of {epikit} is to provide miscellaneous functions for applied
epidemiologists. This is a product of the R4EPIs project; learn more at
<https://r4epis.netlify.app/>.

## Installation

You can install {epikit} from CRAN (see details for the latest version):

``` r
install.packages("epikit")
```

<details>
<!--
NOTE: everything inside the details tag will be collapsed and effectively
hidden from the user
 -->
<summary style="text-decoration: underline">
Click here for alternative installation options
</summary>

If there is a bugfix or feature that is not yet on CRAN, you can install
it via the {drat} package:

You can also install the in-development version from GitHub using the
{remotes} package (but there’s no guarantee that it will be stable):

``` r
# install.packages("remotes")
remotes::install_github("R4EPI/epikit") 
```

</details>

------------------------------------------------------------------------

``` r
library("epikit")
```

The {epikit} was primarily designed to house convenience functions for
applied epidemiologists to use in tidying their reports. The functions
in {epikit} come in a few categories:

## Age categories

A couple of functions are dedicated to constructing age categories and
partitioning them into separate chunks.

-   `age_categories()` takes in a vector of numbers and returns
    formatted age categories.
-   `group_age_categories()` will take a data frame with different age
    categories in columns (e.g. years, months, weeks) and combine them
    into a single column, selecting the column with the lowest priority.

``` r
library("knitr")
library("magrittr")

set.seed(1)
x <- sample(0:100, 20, replace = TRUE)
y <- ifelse(x < 2, sample(48, 20, replace = TRUE), NA)
df <- data.frame(
  age_years = age_categories(x, upper = 80), 
  age_months = age_categories(y, upper = 16, by = 6)
)
df %>% 
  group_age_categories(years = age_years, months = age_months)
#>    age_years age_months age_category
#> 1      60-69       <NA>  60-69 years
#> 2      30-39       <NA>  30-39 years
#> 3        0-9        16+   16+ months
#> 4      30-39       <NA>  30-39 years
#> 5        80+       <NA>    80+ years
#> 6      40-49       <NA>  40-49 years
#> 7      10-19       <NA>  10-19 years
#> 8        80+       <NA>    80+ years
#> 9      50-59       <NA>  50-59 years
#> 10     50-59       <NA>  50-59 years
#> 11       80+       <NA>    80+ years
#> 12       80+       <NA>    80+ years
#> 13     20-29       <NA>  20-29 years
#> 14     50-59       <NA>  50-59 years
#> 15     70-79       <NA>  70-79 years
#> 16       0-9       <NA>    0-9 years
#> 17     70-79       <NA>  70-79 years
#> 18     70-79       <NA>  70-79 years
#> 19       80+       <NA>    80+ years
#> 20     30-39       <NA>  30-39 years
```

## Quick proportions with conficence intervals

There are three functions that will provide quick statistics for
different rates based on binomial estimates of proportions from
`binom::binom.wilson()`

-   `attack_rate()`
-   `case_fatality_rate()`
-   `mortality_rate()`

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
case_fatality_rate_df(ebola_sim_clean$linelist, 
  outcome == "Death", 
  group = gender,
  add_total = TRUE,
  mergeCI = TRUE
)
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `gender = forcats::fct_explicit_na(gender, "(Missing)")`.
#> Caused by warning:
#> ! `fct_explicit_na()` was deprecated in forcats 1.0.0.
#> ℹ Please use `fct_na_value_to_level()` instead.
#> ℹ The deprecated feature was likely used in the epikit package.
#>   Please report the issue at <https://github.com/R4EPI/epikit/issues>.
#> # A tibble: 3 × 5
#>   gender deaths population   cfr ci           
#>   <fct>   <int>      <int> <dbl> <chr>        
#> 1 f        1291       2280  56.6 (54.58-58.64)
#> 2 m        1273       2247  56.7 (54.59-58.69)
#> 3 Total    2564       4527  56.6 (55.19-58.08)
```

## Inline functions

The inline functions make it easier to print estimates with confidence
intervals in reports with the correct number of digits.

-   `fmt_ci()` formats confidence intervals from three numbers.
    (e.g. `fmt_ci(50, 10, 80)` produces 50.00% (CI 10.00-80.00)
-   `fmt_pci()` formats confidence intervals from three fractions,
    multiplying by 100 beforehand.

The `_df` suffixes (`fmt_ci_df()`, `fmt_pci_df()`) will print the
confidence intervals for data stored in data frames. These are designed
to work with the outputs of the rates functions. For example,
`fmt_ci_df(attack_rate(10, 50))` will produce 20.00% (CI 11.24-33.04).
All of these suffixes will have three options `e`, `l`, and `u`. These
refer to `estimate`, `lower`, and `upper` column positions or names.

-   `fmt_count()` will count a condition in a data frame and present the
    number and percent of `TRUE` values. For example, if you wanted to
    count the number of women patients from Rokupa hospital, you would
    write:
    `fmt_count(ebola_sim_clean$linelist, gender == "f", hospital == "Rokupa Hospital")`
    and it would produce: 210 (3.6%)

## Confidence interval manipulation

The confidence interval manipulation functions take in a data frame and
combine their confidence intervals into a single character string much
like the inline functions do. There are two flavors:

-   `merge_ci_df()` and `merge_pci_df()` will merge just the values of
    the confidence interval and leave the estimate alone. Note: this
    WILL remove the lower and upper columns.
-   `unite_ci()` merges both the confidence interval and the estimate
    into a single character column. This generally has more options than
    `merge_ci()`

This is useful for reporting models:

``` r
fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
df  <- data.frame(v = names(coef(fit)), e = coef(fit), confint(fit), row.names = NULL)
names(df) <- c("variable", "estimate", "lower", "upper")
print(df)
#>      variable    estimate        lower       upper
#> 1 (Intercept) 0.740647656 -0.774822875 2.256118188
#> 2        disp 0.002702925 -0.002867999 0.008273849
#> 3          hp 0.005274547 -0.001400580 0.011949674
#> 4          wt 1.001303136  0.380088737 1.622517536
#> 5          am 0.155814790 -0.614677730 0.926307310

# unite CI has more options
unite_ci(df, "slope (CI)", estimate, lower, upper, m100 = FALSE, percent = FALSE)
#>      variable        slope (CI)
#> 1 (Intercept) 0.74 (-0.77-2.26)
#> 2        disp 0.00 (-0.00-0.01)
#> 3          hp 0.01 (-0.00-0.01)
#> 4          wt  1.00 (0.38-1.62)
#> 5          am 0.16 (-0.61-0.93)

# merge_ci just needs to know where the estimate is
merge_ci_df(df, e = 2)
#>      variable    estimate           ci
#> 1 (Intercept) 0.740647656 (-0.77-2.26)
#> 2        disp 0.002702925 (-0.00-0.01)
#> 3          hp 0.005274547 (-0.00-0.01)
#> 4          wt 1.001303136  (0.38-1.62)
#> 5          am 0.155814790 (-0.61-0.93)
```

## Give me a break

If you need a quick function to determine the number of breaks you need
for a grouping or color scale, you can use `find_breaks()`. This will
always start from 1, so that you can include zero in your scale when you
need to.

``` r
find_breaks(100) # four breaks from 1 to 100
#> [1]  1 26 51 76
find_breaks(100, snap = 20) # four breaks, snap to the nearest 20
#> [1]  1 41 81
find_breaks(100, snap = 20, ceiling = TRUE) # include the highest number
#> [1]   1  41  81 100
```

## Pull together population counts

To quickly pull together population counts for use in surveys or
demographic pyramids the `gen_population()` function can help. If you
only know the proportions in each group the function will convert this
to counts for you - whereas if you have counts, you can type those in
directly. The default proportions are based on Doctors Without Borders
general emergency intervention standard values.

``` r
# get population counts based on proportion, stratified
gen_population(groups = c("0-4","5-14","15-29","30-44","45+"), 
               strata = c("Male", "Female"), 
               proportions = c(0.079, 0.134, 0.139, 0.082, 0.067))
#> Warning in gen_population(groups = c("0-4", "5-14", "15-29", "30-44", "45+"), : Given proportions (or counts) is not the same as
#> groups multiplied by strata length, they will be repeated to match
#> # A tibble: 10 × 4
#>    groups strata proportions     n
#>    <fct>  <fct>        <dbl> <dbl>
#>  1 0-4    Male         0.079    79
#>  2 5-14   Male         0.134   134
#>  3 15-29  Male         0.139   139
#>  4 30-44  Male         0.082    82
#>  5 45+    Male         0.067    67
#>  6 0-4    Female       0.079    79
#>  7 5-14   Female       0.134   134
#>  8 15-29  Female       0.139   139
#>  9 30-44  Female       0.082    82
#> 10 45+    Female       0.067    67
```

Type in counts directly to get the groups in a data frame.

``` r
# get population counts based on counts, stratified - type out counts
# for each group and strata
gen_population(groups = c("0-4","5-14","15-29","30-44","45+"), 
               strata = c("Male", "Female"), 
               counts = c(20, 10, 30, 40, 0, 0, 40, 30, 20, 20))
#> # A tibble: 10 × 4
#>    groups strata proportions     n
#>    <fct>  <fct>        <dbl> <dbl>
#>  1 0-4    Male        0.0952    20
#>  2 5-14   Male        0.0476    10
#>  3 15-29  Male        0.143     30
#>  4 30-44  Male        0.190     40
#>  5 45+    Male        0          0
#>  6 0-4    Female      0          0
#>  7 5-14   Female      0.190     40
#>  8 15-29  Female      0.143     30
#>  9 30-44  Female      0.0952    20
#> 10 45+    Female      0.0952    20
```

## Table modification

These functions all modify the appearance of a table displayed in a
report and work best with the `knitr::kable()` function.

-   `rename_redundant()` renames redundant columns with a single name.
    (e.g. `hopitalized_percent` and `confirmed_percent` can both be
    renamed to `%`)
-   `augment_redundant()` is similar to `rename_redundant()`, but it
    modifies the redundant column names (e.g. `hospitalized_n` and
    `confirmed_n` can become `hospitalized (n)` and `confirmed (n)`)
-   `merge_ci()` combines estimate, lower bound, and upper bound columns
    into a single column.

``` r
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
|----:|-------:|-------:|----:|-------:|-------:|
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
|------:|-----:|--------------:|------:|-----:|--------------:|
|     1 | 0.17 |          3.14 |     6 | 1.00 |          6.28 |
|     2 | 0.33 |          3.14 |     5 | 0.83 |          6.28 |
|     3 | 0.50 |          3.14 |     4 | 0.67 |          6.28 |
|     4 | 0.67 |          3.14 |     3 | 0.50 |          6.28 |
|     5 | 0.83 |          3.14 |     2 | 0.33 |          6.28 |
|     6 | 1.00 |          3.14 |     1 | 0.17 |          6.28 |
