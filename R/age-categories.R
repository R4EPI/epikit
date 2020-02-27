#' Create an age group variable
#'
#' @param x Your age variable
#'
#' @param breakers A string. Age category breaks you can define within c().
#' Alternatively use "lower", "upper" and "by" to set these breaks based on a
#' sequence.
#'
#' @param lower A number. The lowest age value you want to consider (default is 0)
#'
#' @param upper A number. The highest age value you want to consider
#'
#' @param by A number. The number of years you want between groups
#'
#' @param separator A character that you want to have between ages in group
#' names. The default is "-" producing e.g. 0-10.
#'
#' @param ceiling A TRUE/FALSE variable. Specify whether you would like the
#' highest value in your breakers, or alternatively the upper value specified,
#' to be the endpoint. This would produce the highest group of "70-80" rather
#' than "80+". The default is FALSE (to produce a group of 80+).
#'
#' @param above.char Only considered when ceiling == FALSE.  A character that
#' you want to have after your highest age group. The default is "+" producing
#' e.g. 80+
#'
#' @return a factor representing age ranges, open at the upper end of the range.
#'
#' @export
#' @examples
#'
#'
#' if (interactive() && require("dplyr") && require("epidict")) {
#' withAutoprint({
#' set.seed(50)
#' dat <- epidict::gen_data("Cholera", n = 100, org = "MSF")
#' ages <- dat %>%
#'   select(starts_with("age")) %>%
#'   mutate(age_years = age_categories(age_years, breakers = c(0, 5, 10, 15, 20))) %>%
#'   mutate(age_months = age_categories(age_months, breakers = c(0, 5, 10, 15, 20))) %>%
#'   mutate(age_days = age_categories(age_days, breakers = c(0, 5, 15)))
#'
#' ages %>%
#'   group_age_categories(years = age_years, months = age_months, days = age_days) %>%
#'   pull(age_category) %>%
#'   table()
#' })
#' }
age_categories <- function(x, breakers = NULL, lower = 0, upper = NULL, by = 10,
                           separator = "-", ceiling = FALSE, above.char = "+") {


  # make sure age variable is numeric
  x <- as.numeric(x)

  breaks_exist <- !is.null(breakers)
  upper_exists <- !is.null(upper)
  wrong_upper  <- upper_exists && (length(upper) > 1 || any(is.na(upper)))
  wrong_breaks <- breaks_exist && (length(breakers) < 3 || any(is.na(breakers)))

  if (!upper_exists && !breaks_exist) {
    stop("one of `breakers` or `upper` must be specified", call. = FALSE)
  }

  if (wrong_upper) {
    stop("please only specify a single upper value", call. = FALSE)
  }

  if (wrong_breaks) {
    stop("please specify at least three breakers", call. = FALSE)
  }

  if (!breaks_exist) {
    breakers <- unique(c(seq(lower, upper, by = by), upper))
  }

  nb <- length(breakers)

  if (ceiling) {
    lower_vals <- breakers[c(-nb, -nb + 1)]
    upper_vals <- breakers[c(-1, -nb)] - 1
    final_val  <- sprintf("%d%s%d", breakers[nb - 1], separator, breakers[nb])
    breakers[nb] <- breakers[nb] + 1L
  } else {
    lower_vals <- breakers[-nb]
    upper_vals <- breakers[-1] - 1
    final_val  <- sprintf("%d%s", breakers[nb], above.char)
    breakers   <- unique(c(breakers, Inf))
  }
  labs <- c(paste(lower_vals, upper_vals, sep = separator), final_val)

  output <- cut(
    x,
    breaks = breakers,
    right = FALSE,
    include.lowest = FALSE,
    labels = labs
  )

  # return variable with groups
  output
}


#' @param dat a data frame with at least one column defining an age category
#'
#' @param years,months,weeks,days the bare name of the column defining years,
#' months, weeks, or days (or NULL if the column doesn't exist)
#'
#' @param one_column if `TRUE` (default), the categories will be joined into a
#' single column called "age_category" that appends the type of age category
#' used. If `FALSE`, there will be one column with the grouped age categories
#' called "age_category" and a second column indicating age unit called
#' "age_unit".
#'
#' @param drop_empty_overlaps if `TRUE`, unused levels are dropped if they have
#' been replaced by a more fine-grained definition and are empty. Practically,
#' this means that the first level for years, months, and weeks are in
#' consideration for being removed via [forcats::fct_drop()]
#'
#' @return a data frame
#'
#' @rdname age_categories
#' @export
#'
group_age_categories <- function(dat,
                                 years = NULL,
                                 months = NULL,
                                 weeks = NULL,
                                 days = NULL,
                                 one_column = TRUE,
                                 drop_empty_overlaps = TRUE) {

  if (!is.data.frame(dat)) {
    stop("dat must be a data frame", call. = FALSE)
  }
  # capture the quosures of the columns
  da <- rlang::enquo(days)
  we <- rlang::enquo(weeks)
  mo <- rlang::enquo(months)
  ye <- rlang::enquo(years)

  # check if they are null
  d <- !is.null(rlang::get_expr(da))
  w <- !is.null(rlang::get_expr(we))
  m <- !is.null(rlang::get_expr(mo))
  y <- !is.null(rlang::get_expr(ye))

  # stop if none of them are filled
  if (!d && !w && !m && !y) {
    stop("please specify one or more columns", call. = FALSE)
  }

  # get the columns OR replace them with NA
  nas <- factor(NA_character_)
  da <- if (d) dplyr::pull(dat, !!da) else nas
  we <- if (w) dplyr::pull(dat, !!we) else nas
  mo <- if (m) dplyr::pull(dat, !!mo) else nas
  ye <- if (y) dplyr::pull(dat, !!ye) else nas

  # If there is one column, prepend the levels with the correct designation
  if (one_column) {
    levels(da) <- if (d) paste(levels(da), "days") else levels(da)
    levels(we) <- if (w) paste(levels(we), "weeks") else levels(we)
    levels(mo) <- if (m) paste(levels(mo), "months") else levels(mo)
    levels(ye) <- if (y) paste(levels(ye), "years") else levels(ye)
  } else {
    type <- NULL
  }
  dac <- as.character(da)
  wec <- as.character(we)
  moc <- as.character(mo)
  yec <- as.character(ye)
  # create the resulting column by grabbing first days, weeks, months, and years
  res <- dplyr::case_when(
    !is.na(da) ~ dac,
    !is.na(we) ~ wec,
    !is.na(mo) ~ moc,
    TRUE       ~ yec
  )

  # Combine the levels
  levs <- forcats::lvls_union(list(da, we, mo, ye))
  res  <- factor(res, levels = levs)

  if (drop_empty_overlaps) {
    # Remove any overlapping levels
    droppings <- c(if (d) levels(we)[1] else NA,
                   if (w) levels(mo)[1] else NA,
                   if (m) levels(ye)[1] else NA)
    res       <- forcats::fct_drop(res, droppings)
  }

  # Add the column(s) and return
  if (one_column) {
    res <- tibble::add_column(dat, age_category = res)
  } else {
    type <- dplyr::case_when(
      !is.na(da) ~ "days",
      !is.na(we) ~ "weeks",
      !is.na(mo) ~ "months",
      TRUE       ~ "years"
    )
    type <- forcats::fct_drop(factor(type, c("days", "weeks", "months", "years")))
    res <- tibble::add_column(dat, age_category = res, age_unit = type)
  }
  res

}
