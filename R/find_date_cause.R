#' Find the first date beyond a cutoff in several columns
#'
#' This function will find the first date in an ordered series of columns that
#' falls within a specified period. If no dates from the provided columns fall
#' within the period, it returns the period boundary (start or end) as a fallback.
#'
#' @param x a data frame
#' @param ... an ordered series of date columns (i.e. the most important date
#'   to be considered first). Earlier columns take precedence in case of ties.
#' @param datecol the name of the new column to contain the dates
#' @param datereason the name of the column to contain the name of the column
#'   from which the date came.
#' @param period_start,period_end for the find_ functions, this should be the
#'   name of a column in `x` that contains the start/end of the recall period.
#'   For `constrain_dates`, this should be a vector of dates.
#' @param na_fill one of "start", "end", or NULL. If "start" or "end", NA values
#'   in the result will be replaced with the corresponding period boundary.
#'   If NULL, NAs are left as-is.
#' @export
#'
#' @examples
#' d <- data.frame(
#'   s1 = c(as.Date("2013-01-01") + 0:10, as.Date(c("2012-01-01", "2014-01-01"))),
#'   s2 = c(as.Date("2013-02-01") + 0:10, as.Date(c("2012-01-01", "2014-01-01"))),
#'   s3 = c(as.Date("2013-01-10") - 0:10, as.Date(c("2012-01-01", "2014-01-01"))),
#'   ps = as.Date("2012-12-31"),
#'   pe = as.Date("2013-01-09")
#' )
#' print(dd <- find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe))
#' print(bb <- find_date_cause(d, s1, s2, s3, period_start = ps, period_end = pe,
#'                             na_fill = "end",
#'                             datecol = "enddate",
#'                             datereason = "endcause"))
#' find_date_cause(d, s3, s2, s1, period_start = ps, period_end = pe)
#'
#' # works
#' assert_positive_timespan(dd, start_date, pe)
#'
#' # returns a warning because the last date isn't later than the start_date
#' assert_positive_timespan(dd, start_date, s2)
#'
#'
#' with(d, constrain_dates(s1, ps, pe))
#' with(d, constrain_dates(s2, ps, pe))
#' with(d, constrain_dates(s3, ps, pe))
#'
find_date_cause <- function(x,
                            ...,
                            period_start = NULL,
                            period_end = NULL,
                            datecol = "start_date",
                            datereason = "start_date_reason",
                            na_fill = "start") {

  if (!is.null(na_fill)) {
    na_fill <- match.arg(tolower(na_fill), c("start", "end"))
  }
  .dots        <- tidyselect::vars_select(colnames(x), ...)
  period_start <- rlang::enquo(period_start)
  period_end   <- rlang::enquo(period_end)
  period_start <- tidyselect::vars_select(colnames(x), !! period_start)
  period_end   <- tidyselect::vars_select(colnames(x), !! period_end)


  # date checking with informative error message
  all_cols <- c(.dots, period_start, period_end)
  are_dates <- vapply(x[all_cols], inherits, logical(1), "Date")

  if (!all(are_dates)) {
    non_date_cols <- all_cols[!are_dates]
    stop(sprintf("The following columns are not Date objects: %s",
                 paste(non_date_cols, collapse = ", ")))
  }

  y <- x[c(.dots, period_start, period_end)]

  # Keep original dates to check if we're discarding valid data
  y_original <- y[.dots]

  # removing dates that don't conform
  y <- dplyr::mutate_at(.tbl         = y,
                        .vars        = .dots,
                        .funs        = constrain_dates,
                        period_start = y[[period_start]],
                        period_end   = y[[period_end]],
                        boundary     = "both")

  y_result <- choose_first_good_date(y[.dots])

  # Throw warning of rows where we have ignored dates outside of the period
  warn_ignored_out_of_period(
    original_data = y_original,
    result = y_result[[1]],
    period_start = y[[period_start]],
    period_end = y[[period_end]],
    row_indices = seq_len(nrow(x))
  )

  # Fill NAs with period boundaries if requested
  if (!is.null(na_fill)) {
    to_fill <- is.na(y_result[[1]])
    if (any(to_fill)) {
      boundary_col <- if (na_fill == "start") period_start else period_end
      boundary_name <- if (na_fill == "start") "period_start" else "period_end"
      y_result[[1]][to_fill] <- y[[boundary_col]][to_fill]
      y_result[[2]][to_fill] <- boundary_name

      check_inappropriate_fills(
        original_data = y_original,
        result = y_result[[1]],
        period_start = y[[period_start]],
        period_end = y[[period_end]],
        na_fill = na_fill,
        row_indices = seq_len(nrow(x)),
        na_rows = to_fill
      )
    }
  }

  tibble::add_column(!! rlang::sym(datecol)    := y_result[[1]],
                     !! rlang::sym(datereason) := y_result[[2]],
                     .data   = x,
                     .before = .dots[[1]])
}


#' @rdname find_date_cause
#' @export
find_start_date <- function(x, ...,
                            period_start = NULL,
                            period_end = NULL,
                            datecol = "start_date",
                            datereason = "start_date_reason"
                            ) {

  find_date_cause(x, ...,
                  period_start = !! rlang::enquo(period_start),
                  period_end   = !! rlang::enquo(period_end),
                  datecol      = datecol,
                  datereason   = datereason,
                  na_fill      = "start")

}

#' @rdname find_date_cause
#' @export
find_end_date <- function(x, ...,
                          period_start = NULL,
                          period_end = NULL,
                          datecol = "end_date",
                          datereason = "end_date_reason"
                          ) {

  find_date_cause(x, ...,
                  period_start = !! rlang::enquo(period_start),
                  period_end   = !! rlang::enquo(period_end),
                  datecol      = datecol,
                  datereason   = datereason,
                  na_fill      = "end")

}

#' @rdname find_date_cause
#' @param i a vector of dates
#' @param boundary one of "both", "start", or "end". Dates outside of the
#'   boundary will be set to NA.
#' @export
constrain_dates <- function(i, period_start, period_end, boundary = "both") {

  boundary  <- match.arg(boundary, c("both", "start", "end"))
  nna       <- !is.na(i)
  too_early <- nna & i < period_start
  too_late  <- nna & i > period_end

  if (boundary != "both") {
    at_the_beginning <- boundary == "start"
    # If boundary = "start", replace early dates; if "end", replace late dates
    trim    <- if (at_the_beginning) too_early    else too_late
    repl    <- if (at_the_beginning) period_start else period_end
    i[trim] <- repl

    # Now set the other side to NA
    if (at_the_beginning) {
      i[too_late] <- NA
    } else {
      i[too_early] <- NA
    }
  } else {
    # boundary = "both": set anything outside period to NA
    i[nna & (i < period_start | i > period_end)] <- NA
  }

  i

}

#' Choose the first non-missing date from a data frame of dates
#'
#' @param date_a_frame a data frame where each column contains a different
#'   parsing of the same date vector
#' @keywords internal
#' @noRd
#' @note: This function was written and modified by Zhian N. Kamvar and comes
#'   from the linelist package,
choose_first_good_date <- function(date_a_frame) {
  n   <- nrow(date_a_frame)
  res <- data.frame(the_date = rep(as.Date(NA), length = n),
                    the_col  = character(n),
                    stringsAsFactors = FALSE
                    )
  for (i in seq_len(n)) {
    tmp <- date_a_frame[i, ]
    suppressWarnings(nona <- min(which(!is.na(tmp))))
    if (!is.finite(nona)) {
      next
    }
    res$the_date[i] <- tmp[[nona]]
    res$the_col[i]  <- names(date_a_frame)[nona]
  }
  res
}


#' Warn when out-of-period dates are ignored
#'
#' @description
#' This internal helper is used by [find_date_cause()] to emit a warning when
#' a valid in-period date is selected, but one or more candidate dates in the
#' same row fall outside the defined period.
#'
#' The purpose is to alert users that while the chosen date was within
#' `period_start` and `period_end`, some earlier or later dates were
#' implicitly ignored. This can help identify data quality or sequencing
#' issues when multiple date columns overlap or extend beyond a reporting
#' window.
#'
#' @param original_data A data frame containing the original date columns.
#' @param result A vector of the selected date values (typically from
#'   `find_date_cause()`).
#' @param period_start,period_end Vectors (usually from the same data frame)
#'   defining the lower and upper bounds of the period.
#' @param row_indices Integer vector of row numbers corresponding to the
#'   original data.
#'
#' @details
#' A warning is issued for any row where:
#' \itemize{
#'   \item The chosen date (`result[i]`) falls within the specified period; **and**
#'   \item The original row contained other valid (non-missing) dates that
#'   fell strictly before `period_start[i]` or after `period_end[i]`.
#' }
#'
#' @return
#' Invisibly returns `NULL`. Called for its side effect of issuing warnings.
#'
#' @keywords internal
#' @noRd

warn_ignored_out_of_period <- function(original_data, result, period_start, period_end, row_indices) {
  for (i in seq_len(nrow(original_data))) {
    original_row <- as.Date(as.numeric(original_data[i, ]), origin = "1970-01-01")
    valid_dates <- original_row[!is.na(original_row)]
    if (length(valid_dates) == 0 || is.na(result[i])) next

    # If the result date is within the period, check for other ignored ones
    if (result[i] >= period_start[i] && result[i] <= period_end[i]) {
      has_before <- any(valid_dates < period_start[i])
      has_after  <- any(valid_dates > period_end[i])
      if (has_before || has_after) {
        warning(
          sprintf(
            "Row %d: ignored date(s) %s while selecting in-period date %s",
            row_indices[i],
            paste(
              c(
                if (has_before) "before period_start",
                if (has_after) "after period_end"
              ),
              collapse = " and "
            ),
            result[i]
          ),
          call. = FALSE
        )
      }
    }
  }
}

#' Check for inappropriate NA fills when valid dates exist outside period
#'
#' This function warns when we're filling NAs with period boundaries but the
#' original data had valid dates that were outside the period. This could
#' indicate problematic data (e.g., arrival dates after the survey period).
#'
#' @param original_data data frame with original unconstrained dates
#' @param result vector of result dates (after constraining)
#' @param period_start vector of period start dates
#' @param period_end vector of period end dates
#' @param na_fill either "start" or "end"
#' @param row_indices original row numbers for reporting
#' @param na_rows logical vector indicating which rows had NAs filled
#' @keywords internal
#' @noRd
check_inappropriate_fills <- function(original_data, result, period_start,
                                      period_end, na_fill, row_indices, na_rows) {

  # Only check rows where we actually filled an NA
  rows_to_check <- which(na_rows)

  if (length(rows_to_check) == 0) {
    return(invisible(NULL))
  }

  # For those rows, check if original data had any non-NA dates outside the period
  problematic_rows <- c()

  for (i in rows_to_check) {
    original_row <- original_data[i, ]
    has_valid_data <- any(!is.na(original_row))

    if (has_valid_data) {
      # Check if those dates were outside the period
      if (na_fill == "start") {
        # We filled with start, so check if original dates were after period_end
        dates_after <- original_row[!is.na(original_row)] > period_end[i]
        if (any(dates_after)) {
          problematic_rows <- c(problematic_rows, row_indices[i])
        }
      } else {
        # We filled with end, so check if original dates were before period_start
        dates_before <- original_row[!is.na(original_row)] < period_start[i]
        if (any(dates_before)) {
          problematic_rows <- c(problematic_rows, row_indices[i])
        }
      }
    }
  }

  if (length(problematic_rows) > 0) {
    direction <- if (na_fill == "start") "after period_end" else "before period_start"
    boundary_name <- if (na_fill == "start") "period_start" else "period_end"

    warning(
      sprintf(
        "%d row(s) had valid dates %s but were filled with %s: rows %s",
        length(problematic_rows),
        direction,
        boundary_name,
        paste(problematic_rows, collapse = ", ")
      ),
      immediate. = TRUE,
      call. = FALSE
    )
  }

  invisible(NULL)
}



#' @param date_start,date_end column name of a date vector
#' @rdname find_date_cause
#' @export
assert_positive_timespan <- function(x, date_start, date_end) {

  ds <- tidyselect::vars_select(colnames(x), !! rlang::enquo(date_start))
  de <- tidyselect::vars_select(colnames(x), !! rlang::enquo(date_end  ))
  res <- x[[de]] - x[[ds]]
  all_right <- all(res >= 0, na.rm = TRUE)
  if (!all_right) {
    y <- x[res < 0, , drop = FALSE]
    warning(sprintf("%d rows had negative timespans", nrow(y)), immediate. = TRUE)
    return(y)
  }
  return(invisible(NULL))
}
