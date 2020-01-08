#' create factors from numbers
#'
#' If the number of unique numbers is five or fewer, then they will simply
#' be converted to factors in order, otherwise, they will be passed to cut and
#' pretty, preserving the lowest value.
#'
#' @param x a vector of integers or numerics
#'
#' @export
#' @return a factor
#'
#' @examples
#' fac_from_num(1:100)
#' fac_from_num(sample(100, 5))
fac_from_num <- function(x) {
  if (!inherits(x, c("numeric", "integer"))) {
    stop("fac_from_num() only uses integer or numeric vectors.", call. = FALSE)
  }  
  # count the number of unique numbers
  udc <- sort(unique(x))
  udc <- as.character(udc)

  if (length(udc) < 6) {
    x <- factor(as.character(x), levels = udc)
  } else {
    x <- cut(x,
      breaks = pretty(range(x, na.rm = TRUE)),
      include.lowest = TRUE
    )
  }
  x
}
