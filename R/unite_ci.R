#' Unite estimates and confidence intervals
#' 
#' create a character column by combining estimate, lower and upper columns. 
#' This is similar to [tidyr::unite()].
#'
#' @param x a data frame with at least three columns defining an estimate, lower
#' bounds, and upper bounds.
#' @param col the quoted name of the replacement column to create
#' @param ... three columns to bind together in the order of Estimate, Lower, and
#' Upper.
#' @param remove if `TRUE` (default), the three columns in `...` will be replaced by `col`
#' @param digits the number of digits to retain for the confidence interval.
#' @param m100 `TRUE` if the result should be multiplied by 100
#' @param percent `TRUE` if the result should have a percent symbol added.
#' @param ci `TRUE` if the result should include "CI" within the braces (defaults to FALSE)
#' 
#' @return a modified data frame with merged columns or one additional column
#'   representing the estimate and confidence interval
#'
#' @export
#' @examples
#'
#' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
#' df  <- data.frame(v = names(coef(fit)), e = coef(fit), confint(fit), row.names = NULL)
#' names(df) <- c("variable", "estimate", "lower", "upper")
#' print(df)
#' unite_ci(df, "slope (CI)", estimate, lower, upper, m100 = FALSE, percent = FALSE)
#'
unite_ci <- function(x, col = NULL, ..., remove = TRUE, digits = 2, m100 = TRUE, percent = FALSE, ci = FALSE) {

  from_vars <- tidyselect::vars_select(colnames(x), ...)
  if (length(from_vars) != 3) {
    stop("This function requires three columns: an estimate, a lower value, and an upper value", call. = FALSE)
  }
  if (is.null(col)) {
    col <- from_vars[1]
    col <- if (remove) col else sprintf("%s_ci", col)
  }
  col <- rlang::ensym(col)
  out <- x
  if (remove) {
    out <- out[setdiff(names(out), from_vars)]
  }
  first_pos <- which(names(x) %in% from_vars)[1]
  last_pos  <- which(names(x) %in% from_vars)[3]

  if (m100) {
    new_col <- fmt_pci_df(x, e = from_vars[1], l = from_vars[2], u = from_vars[3], digits = digits, percent = percent)
  } else {
    new_col <- fmt_ci_df(x, e = from_vars[1], l = from_vars[2], u = from_vars[3], digits = digits, percent = percent)
  }
  # remove the CI label if needed
  new_col <- if (ci) new_col else gsub("\\(CI ", "(", new_col)
  after   <- if (remove) first_pos - 1L else last_pos
  out     <- tibble::add_column(out, !! col := new_col, .after = after, .name_repair = "minimal")

  out
}

#' @export
#' @inheritParams fmt_pci_df
#' @rdname unite_ci
merge_ci_df <- function(x, e = 3, l = e + 1, u = e + 2, digits = 2) {
  cis <- fmt_ci_df(x, e, l, u, digits)
  x[c(l, u)] <- NULL
  x$ci <- gsub("^.+?\\(CI ", "(", cis)
  x
}

#' @export
#' @rdname unite_ci 
merge_pci_df <- function(x, e = 3, l = e + 1, u = e + 2, digits = 2) {
  cis <- fmt_pci_df(x, e, l, u, digits)
  x[c(l, u)] <- NULL
  x$ci <- gsub("^.+?\\(CI ", "(", cis)
  x
}

